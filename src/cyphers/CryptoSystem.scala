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
//
// abstract classes of Key, CryptoSystem, EncapsulatedCrypto and Opponent

import perso.utils.NetworkTools._

abstract class Key [T]
{
  def getPublic(): T
  def getPrivate(): T
  def getString(k: T): String
  def fromString(s: String): T
}

abstract class CryptoSystem [T]
{
  def _encrypt (msg: Array[Byte], key: T, seed: Long): Array[Byte]
  def _decrypt (msg: Array[Byte], key: T): Array[Byte]

  def encrypt (msg: String , key: T, seed: Long = 0): String =
    arrayToNetwork(_encrypt(hostToArray(msg), key, seed))
  def decrypt (msg: String , key: T): String =
    arrayToHost(_decrypt(networkToArray(msg), key))
}

trait EncapsulatedCrypto
{
  type T
  // la string est l'information à donner sur le réseau
  //(clé publique pour RSA et ElGamal)
  def makeKey(seed: Long) : (Key[T], String)
  def keyToString(k: T) : String =
  {
    val dummyKey = makeKey(0)._1
    dummyKey.getString(k)
  }
  def stringToKey(s: String) : T =
  {
    val dummyKey = makeKey(0)._1
    dummyKey.fromString(s)
  }
  def encrypt(msg: String, key: T, seed: Long) : String
  def decrypt(msg: String, key: T) : String
}

trait Opponent
{
  def openEnc(crypt: Array[Byte], infos : Term):Array[Byte]
}

class DummyOpponent extends Opponent
{
  def openEnc(crypt: Array[Byte], infos : Term):Array[Byte] = crypt
}

