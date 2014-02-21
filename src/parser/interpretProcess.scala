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
import scala.collection.mutable
import scala.collection.mutable.SynchronizedQueue


class Channel(c: String)
{
  val name: String = c
  def RetString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
}


class ChannelSet
{
  val allChannels: Set[Channel] = Set[Channel]()
  def Contains(x: Channel): Boolean = allChannels.contains(x)
  def Append(c: Channel): Unit = allChannels += c
}

class Environement
{
  var content: Map[String,Int] = Map[String,Int]()
  def Append(name: String, value: Int): Unit =
  {
      content += (name -> value)
  }

}

class Branch(pList: Process)
{
  var processList = pList
  val environement = new Environement
}



//type Strategy = (Int, Int) => Int
//
//class Context(computer: Strategy) {
//  def interpret(a: Int, b: Int)  { computer(a, b) }
//}

//val add: Strategy = _ + _
//val multiply: Strategy = _ * _

//new Context(multiply).interpret(2, 3)

class foo{

case class InterpretationError() extends Exception
val i = new Interpretor()
def interpretProcess(b: Branch, channels: ChannelSet): Unit =
{
  b.processList match
  {
    case PTrivial() => ()
    case PNew(name, nextProc) =>
    {
      b.environement.Append(name.s, Random.nextInt);
      b.processList = nextProc;
      interpretProcess(b, channels);
    }
    case POut(currentChannel, term, nextProc) =>
    {
      channels.Append(currentChannel); // If already exists overides it.
      currentChannel.content += i.interpretTerm(term);
      b.processList = nextProc;
      interpretProcess(b, channels);
    }
    case PIn(currentChannel, vari, nextProc) =>
    {
      if (!channels.Contains(currentChannel)) throw new InterpretationError
      b.environement.Append(i.interpretTerm(vari), currentChannel.content.dequeue());
      b.processList = nextProc;
      interpretProcess(b, channels);
    }
    case PIf (value, processTrue, processFalse) =>
    {
      if (i.IntToBool(i.interpretValue(value)))
      {
        b.processList = processTrue;
        interpretProcess(b, channels);
      }
      else
      {
        b.processList = processFalse;
        interpretProcess(b, channels)
      }

    }
    //case PInk(currentChannel, vari, term, y: TVar, k: Int, p: Process)
  }
}

}