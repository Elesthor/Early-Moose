////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [Interpretor.scala]                                              //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//                                              \                             //
//                                               \   \_\_    _/_/             //
//                                                \      \__/                 //
//                                                  ---  (oo)\_______   /     //
//                                                       (__)\       )\/      //
//                                                           ||-----||        //
//                                                           ||     ||        //
////////////////////////////////////////////////////////////////////////////////

import util.Random
import scala.collection.mutable.Set

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               InterpretThread                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
//
// Create one thread to interpret a unique branch of a MetaProc.

class InterpretThread
  (interpreter: Interpretor, proc: Process) extends Thread
{
  override def run()
  {
    interpreter.interpretProcess(proc)
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Interpretor                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Main class of the interpretation system.
//  * Contains every interpreting function, from the deepest to the top-level:
//      - Interpretation of values
//      - Interpretation of terms, which contains values
//      - Interpretation of process, whose args are terms.
//      - Interpretation of MetaProc, which enclapsulates some process branches.
//
class Interpretor(synchrone: Boolean)
{

  // Definition of errors exception.
  case class InterpretationError() extends Exception
  case class ListExpected() extends Exception
  case class ValueExpected() extends Exception

  // set of channels
  var channels = Map[String, Channel]()

////////////////////////////////////////////////////////////////////////////////
//                            Utilities function                              //
////////////////////////////////////////////////////////////////////////////////
  def boolToInt (b: Boolean): Int = if (b) 1 else 0
  def intToBool (x: Int): Boolean = if (x==0) false else true

  // Verif wether x represents an int.
  def isInt (x: String): Boolean =
  {
    try
    {
      x.toInt;
      return true // If toInt has not raise an exception, x is an actual int.
    } catch
    {
      case _: java.lang.NumberFormatException => false
    }
  }

  // Verif wether the string x is a non null int.
  def isTrueInt(x:String): Boolean =
  {
    isInt(x) && x.toInt != 0
  }

  // Used to extract args from terms in the form : XXX(.., ..)
  def parseTermFromString(s: String):Term =
  {
    if(s == "err" || s == "")
      throw InterpretationError()
    val p = new Parser(new InputFromString(s+")"))
    val t = p.parseTerm()
    if(p.checkEnd()) // all the term has been read
      return t
    else
      throw InterpretationError()
  }

  // Return a value nested in a TValue, or throw a ValueExpected
  def inTValue(t: Term): Value =
  {
    t match
    {
      case TValue(v) => v
      case _ => throw new ValueExpected()
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                            InterpretValues                                 //
////////////////////////////////////////////////////////////////////////////////
  def interpretValue(v: Value): Int =
  {
    v match
    {
      case VInt(x) => x

      case VCount(li) => li match
      {
        // Remove everything except nul integers and count them
        case ListTerm (l) => ((l.map(interpretTerm)).filter(isTrueInt)).length
        case _            => throw new ListExpected()
      }
      case VSup (left, right) => boolToInt (interpretValue(inTValue(left)) >
                                            interpretValue(inTValue(right)))
      case VEqual (left, right) =>
        // Two errs are not equal
        if (interpretTerm(left) == "err" || interpretTerm(right) == "err")
        {
          throw new InterpretationError()
        }
        boolToInt (interpretTerm(left) == interpretTerm(right))
      case VAnd (left, right) =>
          boolToInt(intToBool(interpretValue(inTValue(left)))
                 && intToBool(interpretValue(inTValue(right))))
      case VOr (left, right) =>
          boolToInt(intToBool(interpretValue(inTValue(left)))
                 || intToBool(interpretValue(inTValue(right))))
      case VNot (v) =>
        boolToInt(!intToBool(interpretValue(inTValue(v))))
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                               InterpretTerm                                //
////////////////////////////////////////////////////////////////////////////////
  def interpretTerm (t: Term): String =
  {
    try
    {
      t match {

        case TVar (p)           => p

        case TValue (v)         => interpretValue(v).toString

        case TPair(left, right) =>
                        "pair("+interpretTerm(left)+","+interpretTerm(right)+")"

        case TPi1 (t)           =>
        {
          parseTermFromString(interpretTerm(t)) match
          {
            case TPair(u1, _) => u1.toString
            case _ => throw new InterpretationError()
          }
        }

        case TPi2 (t) =>
        {
          parseTermFromString(interpretTerm(t)) match
          {
            case TPair(_, u2) => u2.toString
            case _ => throw new InterpretationError()
          }
        }

        case TEnc (left, right) =>
        {
          "enc("+interpretTerm(left)+","+interpretTerm(right)+")"
        }

        case TDec (left, right) =>
        {
          ( parseTermFromString(interpretTerm(left)),
            parseTermFromString(interpretTerm(right)) ) match
          {
            case (TEnc(msg, TPk(TValue(VInt(n1)))), TSk(TValue(VInt(n2)))) if n1 == n2 =>
              msg.toString
            case _ => throw new InterpretationError()
          }
        }

        case TPk  (v) => "pk("+interpretValue(inTValue(v))+")"

        case TSk  (v) => "sk("+interpretValue(inTValue(v))+")"

        case TOpenEnc (t) => interpretTerm(t) // TODO

        case ListTerm (l) => l.map(interpretTerm).toString
      }
    } catch
    {
      case InterpretationError()
      | ListExpected()
      | ValueExpected() => "err"
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretProcess                               //
////////////////////////////////////////////////////////////////////////////////
  def interpretProcess(proc: Process): Unit =
  {
    proc match
    {
      case PTrivial() => ()

      case PNew(name, nextProc) =>
      {
        val randVal = new TValue(new VInt (Random.nextInt))
        val next = nextProc.replace(name.s, randVal)
        interpretProcess(next)
      }

      case POut(currentChannel, term, nextProc) =>
      {
        channels.get(currentChannel).get.push(interpretTerm(term))
        interpretProcess(nextProc)
      }

      case PIn(currentChannel, vari, nextProc) =>
      {
        val varIn = parseTermFromString(channels.get(currentChannel).get.pop)
        val next = nextProc.replace(vari.p, varIn)
        interpretProcess(next)
      }

      case PIf (value, procTrue, procFalse, nextProc) =>
      {
        val execTrue = try
        {
          intToBool(interpretValue(inTValue(value)))
        } catch
        {
          case InterpretationError()
          | ListExpected()
          | ValueExpected() => false
        }
        if(execTrue)
          interpretProcess(procTrue)
        else
          interpretProcess(procFalse)
        interpretProcess(nextProc)
      }

      case PInk(currentChannel, x, u, y, k, nextProc) =>
      {
        val chan = channels.get(currentChannel).get
        // Will contains the result of the k In.
        var li = List[Term]()
        for(i <- 1 to k)
        {
          val t = parseTermFromString(chan.pop)
          li = (u.replace(x.p,t))::li
        }
        val liTerm = new ListTerm(li)
        var next = nextProc.replace(y.p, liTerm)
        interpretProcess(next)
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                            Create channels Map                             //
////////////////////////////////////////////////////////////////////////////////

  def createChannels(program: MetaProc): Map[String, Channel] =
  {
    var channels = Map[String, Channel]()

    // Record a channel
    def setChannel(c: String):Unit =
    {
      channels.get(c) match
      {
        case None =>
          channels += (c -> new Channel(c, synchrone))
        case Some(chan) => ()
      }
    }
    // Pass through a metaproc
    def crossMetaProc(metaP: MetaProc):Unit =
    {
      crossProcess(metaP.pLeft)
      if (metaP.metaPRight.isDefined)
        crossMetaProc(metaP.metaPRight.get)
    }
    // Pass through a process
    def crossProcess(p: Process):Unit =
    {
      p match
      {
        case PTrivial() => ()
        case PIn(c, _, p) =>
          setChannel(c);
          crossProcess(p)
        case PInk(c, _, _, _, _, p) =>
          setChannel(c);
          crossProcess(p)
        case POut(c, _, p) =>
          setChannel(c);
          crossProcess(p)
        case PIf(_, pIf, pElse, p) =>
          crossProcess(pIf);
          crossProcess(pElse);
          crossProcess(p);
        case PNew(_, p) =>
          crossProcess(p);
      }
    }
    crossMetaProc(program)
    return channels
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretMetaProc                              //
////////////////////////////////////////////////////////////////////////////////


  def interpret(metaP: MetaProc)
  {
    println("-- Interpreting --")
    // Set map of channels
    channels = createChannels(metaP)

    // Launch processes
    def auxInterpretor(metaP: MetaProc)
    {
      var i = 0
      for (i <- 1 to metaP.k)
      {
        val left = new InterpretThread(this, metaP.pLeft)
        left.start
      }
      if (metaP.metaPRight.isDefined)
      {
        auxInterpretor(metaP.metaPRight.get)
      }
    }
    auxInterpretor(metaP)
  }
}

