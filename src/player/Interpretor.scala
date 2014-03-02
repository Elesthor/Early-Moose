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
  case class SyntaxError() extends Exception
  case class ListExpected() extends Exception
  case class ValueExpected() extends Exception

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
      case _: Throwable => false
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
    (new Parser(new InputFromString(s+")"))).parseTerm()
  }

  // Return the strategy to use according to the option fiven to the interpretor
  def strategy(): ChannelHandler =
  {
    if (synchrone)  SynchroneStrategy
    else            AsynchroneStrategy
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
          throw new SyntaxError()
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
            case _ => throw new SyntaxError()
          }
        }

        case TPi2 (t) =>
        {
          parseTermFromString(interpretTerm(t)) match
          {
            case TPair(_, u2) => u2.toString
            case _ => throw new SyntaxError()
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
            case (TEnc(msg, TPk(TValue(VInt(n1)))), TSk(TValue(VInt(n2)))) =>
              if(n1!=n2) throw new SyntaxError()
              msg.toString
            case _ => throw new SyntaxError()
          }
        }

        case TPk  (v) => "pk("+interpretValue(inTValue(v))+")"

        case TSk  (v) => "sk("+interpretValue(inTValue(v))+")"

        case ListTerm (l) => l.map(interpretTerm).toString
      }
    }
    catch {case _: Throwable => "err"}
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretProcess                               //
////////////////////////////////////////////////////////////////////////////////
  def interpretProcess(proc: Process): Unit =
  {
    // * proc : current process to be analysed
    // * channels : set of channel which process may interact with
    // * fellow : processes which are modified at the same time as proc
    //      (used when interpreting Pseq.)
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
        currentChannel.setStrategy(strategy())
        if (currentChannel.name == "stdout") // Stdout puts the msg on the screen.
        {
          println(interpretTerm(term))
        }
        else
        {
          currentChannel.push(interpretTerm(term))
        }
        interpretProcess(nextProc)
      }

      case PIn(currentChannel, vari, nextProc) =>
      {
        val varIn = parseTermFromString(currentChannel.pop)
        val next = nextProc.replace(vari.p, varIn)
        interpretProcess(next)
      }

      case PIf (value, procTrue, procFalse, nextProc) =>
      {
        if (intToBool(interpretValue(inTValue(value))))
        {
          interpretProcess(procTrue)
        }
        else
        {
          interpretProcess(procFalse)
        }
        interpretProcess(nextProc)
      }

      case PInk(currentChannel, x, u, y, k, nextProc) =>
      {
        // Will contains the result of the k In.
        var li = List[Term]()
        for(i <- 1 to k)
        {
          val t = parseTermFromString(currentChannel.pop)
          li = (u.replace(x.p,t))::li
        }
        val liTerm = new ListTerm(li)
        var next = nextProc.replace(y.p, liTerm)
        interpretProcess(next)
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretMetaProc                              //
////////////////////////////////////////////////////////////////////////////////
  def interpretMetaProc(metaP: MetaProc)
  {
    println("-- Interpreting --")
    val interpret = new Interpretor(synchrone)
    def auxInterpretor(metaP: MetaProc)
    {
      var i = 0
      for (i <- 1 to metaP.k)
      {
        val left = new InterpretThread(interpret, metaP.pLeft)
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

