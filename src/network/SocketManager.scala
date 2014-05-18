import java.net._
import java.io._
import scala.io._
import java.util.concurrent.Semaphore

class PacketManager
{
  // return the string and the new iterator
  def readPacket(in: collection.Iterator[Char]): (String, collection.Iterator[Char]) =
  {
    // in doc : after takeWhile, the iterator is discarded, so we duplicate it
    val (dup1, dup2) = in.duplicate
    val n = dup1.takeWhile(_ != '#').mkString
    (dup1.take(n.toInt).mkString, dup2.drop(n.length + n.toInt + 2)) // 2 : # and final \n
  }

  def sendPacket(msg: String, out: PrintStream): Unit =
  {
    out.println(msg.length + "#" + msg)
    out.flush()
  }
}

abstract class SocketManager
{
  val in: Iterator[Char]
  val out: PrintStream
  var currentIn = in
  
  val pm = new PacketManager()
  val sempush = new Semaphore(1, true) // on push at once
  val sempop =  new Semaphore(1, true) // idem on pop
  
  def push(msg: String) =
  {
    sempush.acquire()
    pm.sendPacket(msg, out)
    sempush.release()
  }
  
  def pop() =
  {
    sempop.acquire()
    val r = pm.readPacket(currentIn)
    currentIn = r._2
    sempop.release()
    r._1
  }
  
  def closeSocket()
}

class Server (port: Int) extends
{
  val s = new ServerSocket(port).accept()
  val in = new BufferedSource(s.getInputStream()).iter
  val out = new PrintStream(s.getOutputStream())
} with SocketManager
{
  def closeSocket() =
  {
    s.close
  }
}

class Client (host: String, port: Int) extends
{
  val s = new Socket(InetAddress.getByName(host), port)
  val in = new BufferedSource(s.getInputStream()).iter
  val out = new PrintStream(s.getOutputStream())
} with SocketManager
{
  def closeSocket() =
  {
    s.close
  }
}
