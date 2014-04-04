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
  /*def toString(): String
  def fromString(s: String): T*/
}

abstract class CryptoSystem [T]
{
  def _encrypt (msg: Array[Byte], key: T, seed: Int): Array[Byte]
  def _decrypt (msg: Array[Byte], key: T): Array[Byte]
  
  def encrypt (msg: String , key: Key [T], seed: Int = 0): String =
    arrayToNetwork(_encrypt(hostToArray(msg), key.getPublic, seed))
  def decrypt (msg: String , key: Key [T]): String =
    arrayToHost(_decrypt(networkToArray(msg), key.getPrivate))

  // strings are decoded in UTF-8, but arrays of bytes are sent in ISO-8859-1 which is a injection 1 Byte -> 1 Char
  def hostToArray (msg: String) : Array[Byte] =
    msg.getBytes(java.nio.charset.Charset.forName("UTF-8"))
  def arrayToHost (msg: Array[Byte]) : String =
    new String(msg, java.nio.charset.Charset.forName("UTF-8"))
  def networkToArray (msg: String) : Array[Byte] =
    msg.getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
  def arrayToNetwork (msg: Array[Byte]) : String =
    new String(msg, java.nio.charset.Charset.forName("ISO-8859-1"))
}

