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

import scala.collection.mutable.Set
import scala.collection.mutable.SynchronizedQueue

import java.util.concurrent.Semaphore


case class VoidList() extends Exception

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             Channel handler                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implement the pattern strategy for channels

trait ChannelHandler
{

  def push(content: SynchronizedQueue[String], msg: String): Unit
  def pop(content: SynchronizedQueue[String]): String
}

////////////////////////////////////////////////////////////////////////////////
//                              None Strategy                                 //
////////////////////////////////////////////////////////////////////////////////
//
// Dummy strategy, only used when initializing the channel.

object NoneStrategy extends ChannelHandler
{
  def push(content: SynchronizedQueue[String], msg:String) = ()
  def pop(content: SynchronizedQueue[String]): String = ""
}

////////////////////////////////////////////////////////////////////////////////
//                         Asynchoneous Strategy                              //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the asynchone channel:
//    * pushing is always allowed
//    * a process who wants to get a message have to wait for some while the
//      channel is empty.

object AsynchroneStrategy extends ChannelHandler
{

  def push(content: SynchronizedQueue[String], msg:String) =
  {
    content.enqueue(msg)
  }

  def pop(content: SynchronizedQueue[String]): String =
  {
    try
    {
      Thread.sleep(20) // Avoid to get a 100% CPU infinite loop: let some time
      return content.dequeue() // for other threads to put a msg
    } catch
    {
      case _ : Throwable => pop(content) // Try again...
    }
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

object SynchroneStrategy extends ChannelHandler
{
  val token = new Semaphore(1, true) // Will protect the access to push
  def push(content: SynchronizedQueue[String], msg:String) =
  {
    token.acquire() // Only one thread can write in the channel at one time
    try
    {
      if (content.length > 0)
      {
        Thread.sleep(20) // Avoid to get a 100% CPU infinite loop
        throw new VoidList()
      }
      else
      {
        content.enqueue(msg)
        while (content.length > 0) // Wait for someone to read the message
        {
          Thread.sleep(20)
        }
      }
    } catch
    {
      case _: Throwable => token.release(); push(content, msg)
    }
    token.release()
  }

  def pop(content: SynchronizedQueue[String]): String =
  {
    try
    {
      Thread.sleep(2)
      return content.dequeue()
    } catch
    {
      case _ : Throwable => pop(content)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                Channel                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implements a channel, using a given strategy

class Channel(c: String)
{
  val name: String = c
  // Queue to stock the diffrent messages (in synchrone mode, juste an element
  // is needed).
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
  // Initialize the channel with a trivial strategy
  //    (when creating the channel while parsing)
  var strategy: ChannelHandler = NoneStrategy

  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+name+"\n"

  def setStrategy(s: ChannelHandler) =
  {
    strategy = s
  }

  // Enqueue an element in the queue
  def push(msg: String) =
  {
    strategy.push(content, msg)
  }
  // Dequeue an element
  def pop(): String =
  {
    strategy.pop(content)
  }
}

