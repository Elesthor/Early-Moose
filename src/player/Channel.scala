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
    size.release() // increase size
  }

  def pop(): String =
  {
    size.acquire() // wait until size is positive, then reduce it by one
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
  val ready          = new Semaphore(0, true) // the content is available
  val got            = new Semaphore(0, true) // the content was got
  def push(msg:String) =
  {
    this.synchronized // only one thread by channel can enter here at once
    {
      content = msg
      ready.release() // content availabe
      got.acquire() // wait until it is gotten
    }
  }

  def pop(): String =
  {
    ready.acquire() // wait until a content is available
    val r = content
    got.release() // we got it
    return r
  }
}


////////////////////////////////////////////////////////////////////////////////
//                            Stdout Strategy                                 //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the stdout channel:
//    * pushing redirect to stdout
//    * popping throw an exception

class StdoutStrategy extends ChannelHandler
{
  case class PopStdout() extends Exception
  def push(msg:String) =
  {
    println(msg)
  }

  def pop(): String =
  {
    throw new PopStdout()
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                Channel                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implements a channel, using the right strategy

class Channel(c: String, synchrone: Boolean)
{
  var socket: Option[SocketManager] = None
  val name: String = c
  var strategy: ChannelHandler =
    if(name == "stdout") new StdoutStrategy()
    else if(synchrone)   new SynchroneStrategy()
    else                 new AsynchroneStrategy()

  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+name+"\n"

  // Enqueue an element
  def push(msg: String) =
  {
    socket match
    {
      case None => strategy.push(msg)
      case Some(s) => s.push(msg)
    }
  }
  // Dequeue an element
  def pop() =
  {
    socket match
    {
      case None => strategy.pop()
      case Some(s) => s.pop()
    }
  }
  
  // gestion du réseau
  def connect(host: String, port: Int) =
  {
    println("connexion...")
    socket = Some(new Client(host, port))
  }
  def accept(port: Int) =
  {
    println("acceptation...")
    socket = Some(new Server(port))
  }
}

