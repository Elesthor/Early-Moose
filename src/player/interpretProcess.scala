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
  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
}

class ChannelSet(initialSet: Set[Channel])
{
  var allChannels: Set[Channel] = initialSet
  def contains(x: Channel): Boolean = allChannels.contains(x)
  def append(c: Channel): Unit = allChannels.add(c)
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

class Interpretor(synchrone: Boolean)
{

  case class SyntaxError() extends Exception
  case class InterpretationError() extends Exception
  case class ListExpected() extends Exception

  // Utilities function
  def boolToInt (b: Boolean): Int = if (b) 1 else 0
  def intToBool (x: Int): Boolean = if (x==0) false else true
  def isInt        (x: String): Boolean =
      try {x.toInt; true} catch{ case _: Throwable => false}
  def isTrueInt(x:String): Boolean = isInt(x) && x.toInt != 0

  // Split a string "XXX(.., ..)" at the comma
  // return the two arguments of the pair
  /*def PparseComma(str : String, dropped: Int):  Array[String] =
  {
      def crawler(s: String, acc: String, parenthesisCount: Int): Array[String] =
      {
          if (s.length == 0) throw new SyntaxError()
          else if (s(0) == (',') && parenthesisCount == 0)
              return Array(acc.drop(dropped), s.substring(1,s.length-1))
          else if (s(0) == ('(')) crawler(s.drop(1), acc+s(0), parenthesisCount+1)
          else if (s(0) == (')')) crawler(s.drop(1),acc+s(0), parenthesisCount-1)
          else crawler(s.drop(1),acc+s(0), parenthesisCount)
      }
      return crawler(str,"", -1) // The first comma is after "XXX"
  }*/
  
  def parseTermFromString(s: String):Term =
  {
    (new Parser(new InputFromString(s+")"))).parseTerm() // les termes ne peuvent pas être collé à la fin de l'input, d'où la ) qui est ignorée
  }

////////////////////////////////////////////////////////////////////////////////
//                            InterpretValues                                 //
/////////////////////////////// /////////////////////////////////////////////////
  def interpretValue(v: Value): Int =
  {
    v match
    {
    case VInt(x)               => x
    case VCount(li)            => li match
    {
      // Remove everything except nul integers and count them
      case ListTerm (l) => ((l.map(interpretTerm)).filter(isTrueInt)).length
      case Cons(h,t)    => interpretValue(new VCount((new Cons(h,t)).toList))
      case _            => throw new ListExpected()
    }
    case VSup  (left, right)  => boolToInt (interpretValue(left) >
                                            interpretValue(right))
    case VEqual (left, right) =>
      if (interpretTerm(left) == "err" || interpretTerm(right) == "err") throw new SyntaxError()
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
          /*
          if ((interpretTerm(t)).startsWith("pair("))
            ParseComma(interpretTerm(t), 5)(0)
          else throw new SyntaxError()
          */
        }
        case TPi2 (t)           =>
        {
          parseTermFromString(interpretTerm(t)) match
          {
            case TPair(_, u2) => u2.toString
            case _ => throw new SyntaxError()
          }
          /*
          if ((interpretTerm(t)).startsWith("pair("))
            ParseComma(interpretTerm(t), 5)(1)
          else throw new SyntaxError()
          */
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
          
          /*if (interpretTerm(left).matches("""enc\(.*,pk\(\d+\)\)"""))
          {
            val splittedLeft = ParseComma(interpretTerm(left), 4)
            val splittedRight = interpretTerm(right)
            if (splittedRight.matches("""sk\(\d+\)""") &&
              (splittedLeft(1).substring(3,splittedLeft(1).length-1)).toInt==
                             (splittedRight.substring(3,splittedRight.length-1)).toInt)
                splittedLeft(0)
            else throw new SyntaxError()
          }
            else throw new SyntaxError()*/
        }
        case TPk  (v)           => "pk("+interpretValue(v)+")"
        case TSk  (v)           => "sk("+interpretValue(v)+")"
        case ListTerm (l)       =>
          l.foldLeft(""){(acc, item) => acc+"("+interpretTerm(item)+")::"}.dropRight(2)
        case Cons(h, t)         => interpretTerm((new Cons(h,t)).toList)

      }
    }
    catch {case _: Throwable => "err"}
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
        val next = nextProc.replace(name.s, new TValue(new VInt (Random.nextInt))) // FIXME : remplacer les constantes dans les valeurs
        interpretProcess(next, channels)
      }
      case POut(currentChannel, term, nextProc) =>
      {
        channels.append(currentChannel) // If already exists overides it.
        if (currentChannel.name == "stdout")
        {
          println(interpretTerm(term))
        }
        else
        {
          currentChannel.content += interpretTerm(term)
        }
        interpretProcess(nextProc, channels)
      }
      case PIn(currentChannel, vari, nextProc) =>
      {
        if (!channels.contains(currentChannel)) ()//throw new InterpretationError
        val next = nextProc.replace(vari.p, parseTermFromString(currentChannel.content.dequeue()))
        interpretProcess(next, channels)
      }
      case PIf (value, procTrue, procFalse) =>
      {
        if (intToBool(interpretValue(value)))
        {
          interpretProcess(procTrue, channels)
        }
        else
        {
          interpretProcess(procFalse, channels)
        }
      }
      case PInk(currentChannel, x, u, y, k, nextProc) =>
      {
        //var next: Process = new PTrivial()
        //if (k==1)
        //{
        //  if (!channels.contains(currentChannel)) throw new InterpretationError
        //  val t = new TVar(currentChannel.content.dequeue())
        // // val last = nextProc.replace(y.p, new ListTerm(List(u.replace(x.p,t), (new TVar(y.p)))))
//
        //  val ureplaced = u.replace(x.p,t)
        //  val tmp = new Cons(ureplaced, Some(new Cons(new TVar(y.p), None)))
        //  val next = nextProc.replace(y.p, tmp)
        //}
        //else
        //{
        //  if (!channels.contains(currentChannel)) throw new InterpretationError
        //  val t = new TVar(currentChannel.content.dequeue())
        // // val last = nextProc.replace(y.p, new ListTerm(List(u.replace(x.p,t), (new TVar(y.p)))))
//
        //  val ureplaced = u.replace(x.p,t)
        //  val tmp = new Cons(ureplaced, Some(new Cons(new TVar(y.p), None)))
        //  val last = nextProc.replace(y.p, tmp)
        //  next = new PInk(currentChannel, x, u, y, (k-1), last)
        //}


        if (!channels.contains(currentChannel)) ()
        var li = List[Term]()

        for(i <- 1 to k)
        {
          val t = parseTermFromString(currentChannel.content.dequeue())
          li = (u.replace(x.p,t))::li
        }
        println(interpretTerm(new ListTerm(li)))
        var next = nextProc.replace(y.p, new ListTerm(li))
        interpretProcess(next, channels)
      }
      case PSeq(left, right) =>
      {
        interpretProcess(left, channels)
        interpretProcess(right, channels)
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
    val localChannelSet = new ChannelSet(initialSet)

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
