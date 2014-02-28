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


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             Channel handler                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implement the pattern strategy for channels



class Channel(c: String, strategy: ChannelHandler)
{
  val name: String = c
  var content: SynchronizedQueue[String] = new SynchronizedQueue()

  def push(msg: String)
  {
    strategy.push(msg, content)
  }

  def pop()
  {
    strategy.pop(content)
  }
}

trait ChannelHandler
{

  def retString(x: Int): String
  def push = (String,SynchronizedQueue[String]) => Unit
  def pop = SynchronizedQueue[String] => String
}

class AsynchroneousStrategy extends ChannelHandler
{
  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"

  def push(content: SynchronizedQueue[String], msg:String) =
  {
    content.enqueue(msg)
  }

  def pop(content: SynchronizedQueue[String]): String =
  {
    try
    {
      return content.dequeue()
    }
    catch
    {
      case _ : Throwable => read(sq)
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
