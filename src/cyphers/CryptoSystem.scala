////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [GenericCypher.scala]                                            //
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

abstract class Key [T]
{
  def getPublic(): T
  def getPrivate(): T
  // TODO T => String // pour l'interpréteur
  // TODO : encapsulation pour sécuriser la clé privée ?
}

abstract class CryptoSystem [T]
{
  def _encrypt (msg: Array[Byte], key: T): Array[Byte]
  def _decrypt (msg: Array[Byte], key: T): Array[Byte]
  
  def encrypt (msg: String , key: Key [T]): String =
    arrayToNetwork(_encrypt(hostToArray(msg), key.getPublic))
  def decrypt (msg: String , key: Key [T]): String =
    arrayToHost(_decrypt(networkToArray(msg), key.getPrivate))

  def hostToArray (msg: String) : Array[Byte] =
    msg.getBytes(java.nio.charset.Charset.forName("UTF-8"))
  def arrayToHost (msg: Array[Byte]) : String =
    new String(msg, java.nio.charset.Charset.forName("UTF-8"))
  def networkToArray (msg: String) : Array[Byte] =
    msg.getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
  def arrayToNetwork (msg: Array[Byte]) : String =
    new String(msg, java.nio.charset.Charset.forName("ISO-8859-1"))
}

