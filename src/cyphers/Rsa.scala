////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//              [Rsa.scala]                                                   //
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
//                           Utilities functions                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

import scala.BigInt

abstract class Key [T]
{
  def getPublic(): T
  def getPrivate(): T
}

trait CryptoSystem [T]
{
  def encrypt (msg: String , pub: Key [T] ): String
  def decrypt (msg: String , priv: Key [T] ): String
}

class RsaKey (seed: Int) extends Key [(BigInt, BigInt)]
{
  def generate() =
  {
    val randomizer = new util.Random(seed)
    val p = BigInt.apply(1024, 100, randomizer)
    val q = BigInt.apply(1024, 100, randomizer)
    val n = p*q
    val indEulerN = (p-1)*(q-1)
    
    var e = BigInt.apply(indEulerN.bitLength-1, randomizer)
    while (e.gcd(indEulerN) !=  BigInt.apply(1))
    {
      e = BigInt.apply(indEulerN.bitLength-1, randomizer)
    }
    val d = e.modInverse(indEulerN)

    (n, d, e)
  }
  
  var (n, d, e) = generate()
  def getPublic = (n,e)
  def getPrivate = (n,d)
}

class RSA extends CryptoSystem [(BigInt, BigInt)]
{
  def PKCS1StringToInt(msg: String): BigInt = 
  {
    0
  }

  def encrypt(msg: String, key: Key[(BigInt, BigInt)]): String = 
  {
    val (n, e) = key.getPublic
    (PKCS1StringToInt(msg).modPow(e,n)).toString
  }

  def decrypt(crypt: String, key: Key[(BigInt, BigInt)] ): String = 
  {
    val (n, d) = key.getPrivate
    PKCS1IntToString(BigInt.apply(crypt)).modPow(e,d))
  }

}

val a = new RsaKey(12)
println(a.getPublic) 
