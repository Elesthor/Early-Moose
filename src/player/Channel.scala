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


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             Channel handler                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implement the pattern strategy for channels



class Channel(c: String)
{
  val name: String = c
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
  var strategy: ChannelHandler = NoneStrategy

  def setStrategy(s: ChannelHandler) =
  {
    strategy = s
  }

  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+name+"\n"

  def push(msg: String) =
  {
    strategy.push(content, msg)
  }

  def pop(): String =
  {
    strategy.pop(content)
  }
}

case class VoidList() extends Exception

trait ChannelHandler
{

  def push(content: SynchronizedQueue[String], msg: String): Unit
  def pop(content: SynchronizedQueue[String]): String
}


object NoneStrategy extends ChannelHandler
{
  def push(content: SynchronizedQueue[String], msg:String) = ()
  def pop(content: SynchronizedQueue[String]): String = ""
}

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
      Thread.sleep(20)
      return content.dequeue()
    }
    catch
    {
      case _ : Throwable => pop(content)
    }
  }
}

object SynchroneStrategy extends ChannelHandler
{

  def push(content: SynchronizedQueue[String], msg:String) =
  {
    try
    {
      if (content.length > 0)
      {
        Thread.sleep(20)
        throw new VoidList()
      }
      else
      {
        content.enqueue(msg)
        while (content.length > 0)
        {
          Thread.sleep(20)
        }
      }
    } catch
    {
      case _: Throwable => push(content, msg)
    }
  }

  def pop(content: SynchronizedQueue[String]): String =
  {
    try
    {
      Thread.sleep(20)
      return content.dequeue()
    } catch
    {
      case _ : Throwable => pop(content)
    }
  }
}


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               ChannelSet                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
//
// Represents a set of channel. Used to access to channels while interpreting.

class ChannelSet(initialSet: Set[Channel])
{
  // Content of Set, may be initialized with a non-void set.
  var allChannels: Set[Channel] = initialSet

  def contains(x: Channel): Boolean = allChannels.contains(x)
  // If c already exists, don't append it again.
  def append(c: Channel): Unit = allChannels.add(c)
}
