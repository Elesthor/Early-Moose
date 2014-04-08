////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Cesar.scala]                                                  //
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                Cesar Key                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class CesarKey (seed: Int) extends Key [Byte]
{
  def getPublic = seed.toByte
  def getPrivate = (-getPublic).toByte
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Cesar implementation                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Cesar extends CryptoSystem [Byte]
{
  def _encrypt (msg: Array[Byte], key: Byte, _seed:Int = 0): Array[Byte] =
    msg.map({ x => (x + key).toByte})

  def _decrypt (msg: Array[Byte], key: Byte): Array[Byte] =
    _encrypt (msg, key)
}

object TestCesar
{
  def main(args: Array[String]): Unit =
  {
    val key = new CesarKey(1)
    val gen = new Cesar
    val msg = "asalut les coupains :D !▤"
    println(msg)
    val cypher = gen.encrypt(msg,key)
    println(cypher)
    println(gen.decrypt(cypher, key))
  }
}

