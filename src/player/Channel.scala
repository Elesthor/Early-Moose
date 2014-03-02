////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [Channels.scala]                                                 //
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

import scala.collection.mutable.SynchronizedQueue
import java.util.concurrent.Semaphore


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             Channel handler                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implement the pattern strategy for channels

trait ChannelHandler
{
  def push(msg: String): Unit
  def pop(): String
}

////////////////////////////////////////////////////////////////////////////////
//                         Asynchoneous Strategy                              //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the asynchone channel:
//    * pushing is always allowed
//    * a process who wants to get a message have to wait for some while the
//      channel is empty.

class AsynchroneStrategy extends ChannelHandler
{
  val content = new SynchronizedQueue[String]()
  val size = new Semaphore(0, true) // Size of the queue
  def push(msg:String) =
  {
    content.enqueue(msg)
    size.release()
  }

  def pop(): String =
  {
    size.acquire()
    content.dequeue()
  }
}


////////////////////////////////////////////////////////////////////////////////
//                            Synchroneous Strategy                           //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the synchone channel:
//    * pushing is allowed only if the channel is void
//    * the pushing process will stop until the message is read.
//    * a process who wants to get a message have to wait for some while the
//      channel is empty( ie wait for a process to push a msg )

class SynchroneStrategy extends ChannelHandler
{
  var content:String = ""
  val ready          = new Semaphore(0, true)
  val got            = new Semaphore(0, true)
  def push(msg:String) =
  {
    this.synchronized
    {
      content = msg
      ready.release()
      got.acquire()
    }
  }

  def pop(): String =
  {
    ready.acquire()
    val r = content
    got.release()
    return r
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                Channel                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implements a channel, using a given strategy

class Channel(c: String, synchrone: Boolean)
{
  val name: String = c
  // Initialize the channel with a trivial strategy
  //    (when creating the channel while parsing)
  var strategy: ChannelHandler =
    if(synchrone) new SynchroneStrategy()
    else          new AsynchroneStrategy()

  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+name+"\n"

  // Enqueue an element
  def push(msg: String) = strategy.push(msg)
  // Dequeue an element
  def pop() = strategy.pop()
}

