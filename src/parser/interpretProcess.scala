////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [interpretProcess.scala]                                         //
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
import scala.collection.mutable.SynchronizedQueue

//type Strategy = (Int, Int) => Int
//
//class Context(computer: Strategy) {
//  def interpret(a: Int, b: Int)  { computer(a, b) }
//}

//val add: Strategy = _ + _
//val multiply: Strategy = _ * _

//new Context(multiply).interpret(2, 3)


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Channels                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Channel(c: String)
{
  val name: String = c
  def RetString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
}

class ChannelSet(initialSet: Set[Channel])
{
  var allChannels: Set[Channel] = initialSet
  def Contains(x: Channel): Boolean = allChannels.contains(x)
  def Append(c: Channel): Unit = allChannels.add(c)
}

class InterpretThread(interpreter: Interpretor, proc: Process, channels: ChannelSet) extends Thread
{
  override def run()
  {
    interpreter.interpretProcess(proc, channels)
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Interpretor                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Interpretor(){

  case class SyntaxError() extends Exception
  case class InterpretationError() extends Exception

  // Utilities function
  def BoolToInt (b: Boolean): Int = if (b) 1 else 0
  def IntToBool (x: Int): Boolean = if (x==0) false else true
  def isInt        (x: String): Boolean =
      try {x.toInt;true} catch{ case _: Throwable => false}
  def isPositiveInt(x:String): Boolean = isInt(x) && x.toInt > 0

  // Split a string "XXX(.., ..)" at the comma
  // return the two arguments of the pair
  def parseComma(str : String, dropped: Int):  Array[String] =
  {
      def crawler(s: String, acc: String, parenthesisCount: Int): Array[String] =
      {
          if (s.length == 0) throw new SyntaxError
          else if (s(0) == (',') && parenthesisCount == 0)
              return Array(acc.drop(dropped), s.substring(1,s.length-1))
          else if (s(0) == ('(')) crawler(s.drop(1), acc+s(0), parenthesisCount+1)
          else if (s(0) == (')')) crawler(s.drop(1),acc+s(0), parenthesisCount-1)
          else crawler(s.drop(1),acc+s(0), parenthesisCount)
      }
      return crawler(str,"", -1) // The first comma is after "XXX"
  }

////////////////////////////////////////////////////////////////////////////////
//                            InterpretValues                                 //
////////////////////////////////////////////////////////////////////////////////
  def interpretValue(v: Value): Int =
  {
    v match
    {
    case VInt(x)               => x
    case VCount(li)            => li match
    {
      // Remove everything except positve integers and count them
      case ListTerm (l) => ((l.map(interpretTerm)).filter(isPositiveInt)).length
    }
    case VSup  (left, right)  => BoolToInt (interpretValue(left) >
                                            interpretValue(right))
    case VEqual (left, right) => BoolToInt (interpretTerm(left) ==
                                            interpretTerm(right))
    case VAnd (left, right)   => BoolToInt(IntToBool(interpretValue(left))
                                        && IntToBool(interpretValue(right)))
    case VOr (left, right)    => BoolToInt(IntToBool(interpretValue(left))
                                        || IntToBool(interpretValue(right)))
    case VNot (v)             => BoolToInt(!IntToBool(interpretValue(v)))
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
        case TPair(left, right) => "Pair("+interpretTerm(left)+","+interpretTerm(right)+")"
        case TPi1 (t)           =>
        {
          if ((interpretTerm(t)).startsWith("Pair("))
            parseComma(interpretTerm(t), 5)(0)
          else throw new SyntaxError
        }
        case TPi2 (t)           =>
        {
          if ((interpretTerm(t)).startsWith("Pair("))
            parseComma(interpretTerm(t), 5)(1)
          else throw new SyntaxError
        }
        case TEnc (left, right) =>
        {
          "Enc("+interpretTerm(left)+","+interpretTerm(right)+")"
        }
        case TDec (left, right) =>
        {
          if (interpretTerm(left).matches("""Enc\(.*,Pk\(\d+\)\)"""))
          {
            val splittedLeft = parseComma(interpretTerm(left), 4);
            val splittedRight = interpretTerm(right);
            if (splittedRight.matches("""Sk\(\d+\)""") &&
              (splittedLeft(1).substring(3,splittedLeft(1).length-1)).toInt==
                             (splittedRight.substring(3,splittedRight.length-1)).toInt)
                splittedLeft(0)
            else throw new SyntaxError
          }
            else throw new SyntaxError
        }
        case TPk  (v)           => "Pk("+interpretValue(v)+")"
        case TSk  (v)           => "Sk("+interpretValue(v)+")"
        case ListTerm (l)       => l.map(interpretTerm).toString
      }
    }
    catch {case _: Throwable => return "err"}
  }


////////////////////////////////////////////////////////////////////////////////
//                             InterpretProcess                               //
////////////////////////////////////////////////////////////////////////////////
  def interpretProcess(proc: Process, channels: ChannelSet): Unit =
  {
    proc match
    {
      case PTrivial() => ()
      case PNew(name, nextProc) =>
      {
        val next = nextProc.Replace(name.s, new TValue(new VInt (Random.nextInt)));
        interpretProcess(next, channels);
      }
      case POut(currentChannel, term, nextProc) =>
      {
        channels.Append(currentChannel); // If already exists overides it.
        if (currentChannel.name == "stdout")
        {
          println(interpretTerm(term))
        }
        else
        {
          currentChannel.content += interpretTerm(term);
        }
        interpretProcess(nextProc, channels);
      }
      case PIn(currentChannel, vari, nextProc) =>
      {
        if (!channels.Contains(currentChannel)) ()//throw new InterpretationError
        val next = nextProc.Replace(vari.p, new TVar(currentChannel.content.dequeue()));
        interpretProcess(next, channels);
      }
      case PIf (value, procTrue, procFalse) =>
      {
        if (IntToBool(interpretValue(value)))
        {
          interpretProcess(procTrue, channels);
        }
        else
        {
          interpretProcess(procFalse, channels);
        }

      }
      case PInk(currentChannel, x, u, y, k, nextProc) =>
      {
        var next: Process = new PTrivial();
        if (k==0)
        {
          next = nextProc.Replace(x.p, new ListTerm(List[Term]()));
        }
        else
        {
          if (!channels.Contains(currentChannel)) throw new InterpretationError
          val t = new TVar(currentChannel.content.dequeue());
          val last = nextProc.Replace(y.p, new ListTerm(List(u.Replace(x.p,t),y)));
          next = new PInk(currentChannel, x, u , y, (k-1), last);
        }
        interpretProcess(next, channels);
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretMetaProc                              //
////////////////////////////////////////////////////////////////////////////////
  def interpretMetaProc(metaP: MetaProc)
  {
    println("-- Interpreting --");
    val interpret = new Interpretor()
    var initialSet = Set[Channel]()
    val localChannelSet = new ChannelSet(initialSet)

    def auxInterpretor(metaP: MetaProc, channels: ChannelSet)
    {
      var i = 0;
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