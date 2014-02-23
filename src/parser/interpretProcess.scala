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

class Channel(c: String)
{
  val name: String = c
  def RetString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
}

class ChannelSet
{
  var allChannels: Set[Channel] = Set[Channel]()
  def Contains(x: Channel): Boolean = allChannels.contains(x)
  def Append(c: Channel): Unit = allChannels.add(c)
}



case class InterpretationError() extends Exception
val i = new Interpretor()

class foo(){

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
      if (currentChannel.name == "stdout")
      {
        i.interpretTerm(term)
      }
      else
      {
        channels.Append(currentChannel); // If already exists overides it.
        currentChannel.content += i.interpretTerm(term);
      }
      interpretProcess(nextProc, channels);
    }
    case PIn(currentChannel, vari, nextProc) =>
    {
      if (!channels.Contains(currentChannel)) throw new InterpretationError
      val next = nextProc.Replace(vari.p, new TVar(currentChannel.content.dequeue()));
      interpretProcess(next, channels);
    }
    case PIf (value, procTrue, procFalse) =>
    {
      if (i.IntToBool(i.interpretValue(value)))
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
}


val t = new foo()
val std = new Channel ("stdout");
val p1 = new POut(std, new TVar ("coucou"), new PTrivial())

t.interpretProcess(p1, List(std))

