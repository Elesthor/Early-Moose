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
  (interpreter: Interpretor, proc: Process, channels: ChannelSet) extends Thread
{
  override def run()
  {
    interpreter.interpretProcess(proc, channels, List[Process]())
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


////////////////////////////////////////////////////////////////////////////////
//                            Utilities function                              //
/////////////////////////////// ////////////////////////////////////////////////
  def boolToInt (b: Boolean): Int = if (b) 1 else 0
  def intToBool (x: Int): Boolean = if (x==0) false else true

  // Verif wether x represents an int.
  def isInt (x: String): Boolean =
  {
    try
    {
      x.toInt;
      return true // If toInt has not raise an exception, x is an actual int.
    }
    catch
    {
      case _: Throwable => false
    }
  }

  // Verif wether x is a non null int.
  def isTrueInt(x:String): Boolean =
  {
    isInt(x) && x.toInt != 0
  }

  // Used to extract args from terms in the form : XXX(.., ..)
  def parseTermFromString(s: String):Term =
  {
    (new Parser(new InputFromString(s+")"))).parseTerm()
  }

  def strategy(): ChannelHandler =
  {
    if (synchrone)
    {
      SynchroneStrategy
    }
    else
    {
      AsynchroneStrategy
    }
  }
////////////////////////////////////////////////////////////////////////////////
//                            InterpretValues                                 //
/////////////////////////////// ////////////////////////////////////////////////
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
      case VSup (left, right) => boolToInt (interpretValue(left) >
                                            interpretValue(right))
     case VEqual (left, right) =>
      // Two errs are not equal
      if (interpretTerm(left) == "err" || interpretTerm(right) == "err")
      {
        throw new SyntaxError()
      }
      boolToInt (interpretTerm(left) == interpretTerm(right))
      case VAnd (left, right)   => boolToInt(intToBool(interpretValue(left))
                                        && intToBool(interpretValue(right)))
      case VOr (left, right)    => boolToInt(intToBool(interpretValue(left))
                                        || intToBool(interpretValue(right)))
      case VNot (v)             => boolToInt(!intToBool(interpretValue(v)))
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
        case TPair(left, right) => "pair("+interpretTerm(left)+","+interpretTerm(right)+")"
        case TPi1 (t)           =>
        {
          parseTermFromString(interpretTerm(t)) match
          {
            case TPair(u1, _) => u1.toString
            case _ => throw new SyntaxError()
          }
        }
        case TPi2 (t)           =>
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
          (parseTermFromString(interpretTerm(left)), parseTermFromString(interpretTerm(right))) match
          {
            case (TEnc(msg, TPk(VInt(n1))), TSk(VInt(n2))) =>
              if(n1!=n2) throw new SyntaxError()
              msg.toString
            case _ => throw new SyntaxError()
          }
        }
        case TPk  (v) => "pk("+interpretValue(v)+")"
        case TSk  (v) => "sk("+interpretValue(v)+")"
        case ListTerm (l) => l.map(interpretTerm).toString
      }
    }
    catch {case _: Throwable => "err"}
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretProcess                               //
////////////////////////////////////////////////////////////////////////////////
  def interpretProcess(proc: Process, channels: ChannelSet, fellow : List[Process]): Unit =
  {
    // * proc : current process to be analysed
    // * channels : set of channel which proc may interact with
    // * fellow : processes which are modified at the same time as proc
    //      (used when interpreting Pseq.)
    proc match
    {
      case PTrivial() => ()
      case PNew(name, nextProc) =>
      {
        val randVal = new TValue(new VInt (Random.nextInt))
        val next = nextProc.replace(name.s, randVal) // FIXME : remplacer les constantes dans les valeurs
        interpretProcess(next, channels, fellow.map({x => x.replace(name.s, randVal)}))
      }
      case POut(currentChannel, term, nextProc) =>
      {
        currentChannel.setStrategy(strategy())
        channels.append(currentChannel)
        if (currentChannel.name == "stdout") // Stdout puts the msg on the screen.
        {
          println(interpretTerm(term))
        }
        else
        {
          currentChannel.push(interpretTerm(term))
        }
        interpretProcess(nextProc, channels, fellow)
      }
      case PIn(currentChannel, vari, nextProc) =>
      {
        if (!channels.contains(currentChannel))
        {
          throw new SyntaxError()
        }
        val varIn = parseTermFromString(currentChannel.pop)
        val next = nextProc.replace(vari.p, varIn)
        interpretProcess(next, channels, fellow.map({x => x.replace(vari.p, varIn)}))
      }
      case PIf (value, procTrue, procFalse) =>
      {
        if (intToBool(interpretValue(value)))
        {
          interpretProcess(procTrue, channels, fellow)
        }
        else
        {
          interpretProcess(procFalse, channels, fellow)
        }
      }
      case PInk(currentChannel, x, u, y, k, nextProc) =>
      {

        if (!channels.contains(currentChannel))
        {
          throw new SyntaxError()
        }
        // Will contains the result of the k In.
        var li = List[Term]()
        for(i <- 1 to k)
        {
          val t = parseTermFromString(currentChannel.pop)
          li = (u.replace(x.p,t))::li
        }
        val liTerm = new ListTerm(li)
        var next = nextProc.replace(y.p, liTerm)
        interpretProcess(next, channels, fellow.map({x => x.replace(y.p, liTerm)}))
      }
      case PSeq(left, right) =>
      {
        interpretProcess(left, channels, right::fellow)
        interpretProcess(right, channels, fellow)
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
    var initialSet = Set[Channel]()
    var localChannelSet = new ChannelSet(initialSet)

    def auxInterpretor(metaP: MetaProc, channels: ChannelSet)
    {
      var i = 0
      for (i <- 1 to metaP.k)
      {
        val left = new InterpretThread(interpret, metaP.pLeft, localChannelSet)
        left.start
      }
      if (metaP.metaPRight.isDefined)
      {
        auxInterpretor(metaP.metaPRight.get, channels)
      }
    }
    auxInterpretor(metaP, localChannelSet)
  }
}
