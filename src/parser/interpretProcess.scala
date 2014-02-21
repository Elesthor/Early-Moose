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
  def RetString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
  var content = new SynchronizedQueue()
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


def interpretProcess(b: Branch, channels: Set[Channel]): Unit =
{
  b.processList match
  {
    case PTrivial() => ()
    case PNew(name, nextProc) =>
    {
      b.environement.Append(name.s, Random.nextInt);
      b.processList = nextProc;
      interpretProcess(b, channels)
    }
    case POut(channel, term, nextProc) =>
    {
      b.processList = nextProc;
      interpretProcess(b, channels)
    }
  }
}

}