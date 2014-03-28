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

// TODO : essayer avec un grand seed, comme les char ne sont pas des octets
class CesarKey (seed: Int) extends Key [Byte]
{
  def getPublic = seed.toByte
  def getPrivate = (-getPublic).toByte
}

class Cesar extends CryptoSystem [Byte]
{
  def _encrypt (msg: Array[Byte], key: Byte): Array[Byte] =
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
    
    // TODO : pb :
    //new String(Array[Byte](-0 .. -128)).getBytes donne Array(-17, -65, -67) ASCII ? voir l'encodage
  }
}

