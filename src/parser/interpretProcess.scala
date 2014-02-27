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
    interpreter.InterpretProcess(proc, channels)
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Interpretor                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Interpretor()
{

  case class SyntaxError() extends Exception
  case class InterpretationError() extends Exception
  case class ListExpected() extends Exception

  // Utilities function
  def BoolToInt (b: Boolean): Int = if (b) 1 else 0
  def IntToBool (x: Int): Boolean = if (x==0) false else true
  def IsInt        (x: String): Boolean =
      try {x.toInt;true} catch{ case _: Throwable => false}
  def IsTrueInt(x:String): Boolean = IsInt(x) && x.toInt != 0

  // Split a string "XXX(.., ..)" at the comma
  // return the two arguments of the pair
  def ParseComma(str : String, dropped: Int):  Array[String] =
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
  }

////////////////////////////////////////////////////////////////////////////////
//                            InterpretValues                                 //
/////////////////////////////// /////////////////////////////////////////////////
  def InterpretValue(v: Value): Int =
  {
    v match
    {
    case VInt(x)               => x
    case VCount(li)            => li match
    {
      // Remove everything except nul integers and count them
      case ListTerm (l) => ((l.map(InterpretTerm)).filter(IsTrueInt)).length
      case Cons(h,t)    => InterpretValue(new VCount((new Cons(h,t)).toList))
      case _            => throw new ListExpected() // TODO : donner des infos
    }
    case VSup  (left, right)  => BoolToInt (InterpretValue(left) >
                                            InterpretValue(right))
    case VEqual (left, right) =>
      if (InterpretTerm(left) == "err" || InterpretTerm(right) == "err") throw new SyntaxError
      BoolToInt (InterpretTerm(left) == InterpretTerm(right))
    case VAnd (left, right)   => BoolToInt(IntToBool(InterpretValue(left))
                                        && IntToBool(InterpretValue(right)))
    case VOr (left, right)    => BoolToInt(IntToBool(InterpretValue(left))
                                        || IntToBool(InterpretValue(right)))
    case VNot (v)             => BoolToInt(!IntToBool(InterpretValue(v)))
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                               InterpretTerm                                //
////////////////////////////////////////////////////////////////////////////////
  def InterpretTerm (t: Term): String =
  {
    try
    {
      t match {
        case TVar (p)           => p
        case TValue (v)         => InterpretValue(v).toString
        case TPair(left, right) => "Pair("+InterpretTerm(left)+","+InterpretTerm(right)+")" // FIXME : je pense qu'il ne faut pas de majuscule à Pair
        case TPi1 (t)           =>
        {
          if ((InterpretTerm(t)).startsWith("Pair("))
            ParseComma(InterpretTerm(t), 5)(0)
          else throw new SyntaxError()
        }
        case TPi2 (t)           =>
        {
          if ((InterpretTerm(t)).startsWith("Pair("))
            ParseComma(InterpretTerm(t), 5)(1)
          else throw new SyntaxError()
        }
        case TEnc (left, right) =>
        {
          "Enc("+InterpretTerm(left)+","+InterpretTerm(right)+")"
        }
        case TDec (left, right) =>
        {
          if (InterpretTerm(left).matches("""Enc\(.*,Pk\(\d+\)\)""")) // FIXME : pourquoi ne pas appeler Parser.ParseTerm, vérfier que ça a la bonne tete et utiliser l'objet créé ? je te fais un InputFromString pour l'occaz'
          {
            val splittedLeft = ParseComma(InterpretTerm(left), 4);
            val splittedRight = InterpretTerm(right);
            if (splittedRight.matches("""Sk\(\d+\)""") &&
              (splittedLeft(1).substring(3,splittedLeft(1).length-1)).toInt==
                             (splittedRight.substring(3,splittedRight.length-1)).toInt)
                splittedLeft(0)
            else throw new SyntaxError
          }
            else throw new SyntaxError
        }
        case TPk  (v)           => "Pk("+InterpretValue(v)+")"
        case TSk  (v)           => "Sk("+InterpretValue(v)+")"
        case ListTerm (l)       =>
          "List("+l.foldLeft(""){
                                (acc, item) => acc+ InterpretTerm(item)+","}.dropRight(1)+")"
        case Cons(h, t)         => InterpretTerm((new Cons(h,t)).toList)

      }
    }
    catch {case _: Throwable => return "err"}
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretProcess                               //
////////////////////////////////////////////////////////////////////////////////
  def InterpretProcess(proc: Process, channels: ChannelSet): Unit =
  {
    proc match
    {
      case PTrivial() => ()
      case PNew(name, nextProc) =>
      {
        val next = nextProc.Replace(name.s, new TValue(new VInt (Random.nextInt))); // FIXME : confusion entre TVar et VConst lors du parsing
        InterpretProcess(next, channels);
      }
      case POut(currentChannel, term, nextProc) =>
      {
        channels.Append(currentChannel); // If already exists overides it.
        if (currentChannel.name == "stdout")
        {
          println(InterpretTerm(term))
        }
        else
        {
          currentChannel.content += InterpretTerm(term);
        }
        InterpretProcess(nextProc, channels);
      }
      case PIn(currentChannel, vari, nextProc) =>
      {
        if (!channels.Contains(currentChannel)) ()//throw new InterpretationError
        val next = nextProc.Replace(vari.p, new TVar(currentChannel.content.dequeue()));
        InterpretProcess(next, channels);
      }
      case PIf (value, procTrue, procFalse) =>
      {
        if (IntToBool(InterpretValue(value)))
        {
          InterpretProcess(procTrue, channels);
        }
        else
        {
          InterpretProcess(procFalse, channels);
        }
      }
      case PInk(currentChannel, x, u, y, k, nextProc) =>
      {
        //var next: Process = new PTrivial();
        //if (k==1)
        //{
        //  if (!channels.Contains(currentChannel)) throw new InterpretationError
        //  val t = new TVar(currentChannel.content.dequeue());
        // // val last = nextProc.Replace(y.p, new ListTerm(List(u.Replace(x.p,t), (new TVar(y.p)))));
//
        //  val uReplaced = u.Replace(x.p,t)
        //  val tmp = new Cons(uReplaced, Some(new Cons(new TVar(y.p), None)))
        //  val next = nextProc.Replace(y.p, tmp);
        //}
        //else
        //{
        //  if (!channels.Contains(currentChannel)) throw new InterpretationError
        //  val t = new TVar(currentChannel.content.dequeue());
        // // val last = nextProc.Replace(y.p, new ListTerm(List(u.Replace(x.p,t), (new TVar(y.p)))));
//
        //  val uReplaced = u.Replace(x.p,t)
        //  val tmp = new Cons(uReplaced, Some(new Cons(new TVar(y.p), None)))
        //  val last = nextProc.Replace(y.p, tmp);
        //  next = new PInk(currentChannel, x, u, y, (k-1), last);
        //}


        if (!channels.Contains(currentChannel)) ()
        var li = List[Term]()

        for(i <- 1 to k)
        {
          val t = new TVar(currentChannel.content.dequeue()) // FIXME : pourquoi c'est une variable ??
          li = (u.Replace(x.p,t))::li
        }
        println(InterpretTerm(new ListTerm(li)))
        var next = nextProc.Replace(y.p, new ListTerm(li));
        InterpretProcess(next, channels);
      }
      case PSeq(left, right) =>
      {
        InterpretProcess(left, channels);
        InterpretProcess(right, channels);
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             InterpretMetaProc                              //
////////////////////////////////////////////////////////////////////////////////
  def InterpretMetaProc(metaP: MetaProc)
  {
    println("-- Interpreting --");
    val interpret = new Interpretor()
    var initialSet = Set[Channel]()
    val localChannelSet = new ChannelSet(initialSet)

    def AuxInterpretor(metaP: MetaProc, channels: ChannelSet)
    {
      var i = 0;
      for (i <- 1 to metaP.k)
      {
        val left = new InterpretThread(interpret, metaP.pLeft, localChannelSet)
        left.start
      }
      if (metaP.metaPRight.isDefined)
      {
        AuxInterpretor(metaP.metaPRight.get, channels)
      }
    }
    AuxInterpretor(metaP, localChannelSet)
  }
}
