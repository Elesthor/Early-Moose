////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//              [AES.scala]                                                   //
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

import scala.BigInt
import scala.math

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                              Key Generator                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

abstract class Key [T]
{
  def getPublic(): T
  def getPrivate(): T
  // TODO T => String // pour l'interpréteur
  // TODO : encapsulation pour sécuriser la clé privée ?
}


class AESKey extends Key [Int]
{
  def generate() =
  {

  }

}

class AESState(input: BigInt)
{
  def init(inputBlock: BigInt) = 
  {
    val inputBytes = bigInt.toByteArray(inputBlock)
    Array.tabulate[Byte](4,4) { (i,j) => inputBytes(i+4*j)  }
  }
  
  val currentState = init(input)

  def shiftRows() = 
    for (i<- 0 to 3) currentState(i).map{x=>(x-i+4)%4}

}
