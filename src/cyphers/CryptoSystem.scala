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
// abstract classes of Key and CryptoSystem

import perso.utils.NetworkTools._

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
}

trait EncapsulatedCrypto
{
  type T
  def makeKey(seed: Int) : Key[T]
  def encrypt(msg: String, key: Key[T]) : String
  def decrypt(msg: String, key: Key[T]) : String
}

