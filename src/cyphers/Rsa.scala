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

val PQ_LENGTH = 1024

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
    val p = BigInt.apply(PQ_LENGTH, 100, randomizer)
    val q = BigInt.apply(PQ_LENGTH, 100, randomizer)
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
  // Return a padded representation of the byte at len digit.
  def padByte(s: String , len: Int): String = 
  {
    "0"*(len-s.toString.length)+s.toString
  }

  def PKCS1StringToInt(msg: String): String  = 
  {
    (msg.getBytes.map({x => padByte(x.toString, 3)})).foldLeft(""){(s,c)=>s+c}
  }

  def PKCS1IntToString(nb: BigInt): String = 
  {
    (nb.toString.grouped(3).toArray.map(_.toInt.toChar).foldLeft(""){(s,c)=>s+c})
  }

  def encrypt(msg: String, key: Key[(BigInt, BigInt)]): String = 
  {
    val (n, e) = key.getPublic
    val chunks = PKCS1StringToInt(msg).grouped(PQ_LENGTH).toArray  
    chunks.map({x => padByte(BigInt.apply(x).modPow(e,n).toString, PQ_LENGTH)}).foldLeft(""){(s,c)=>s+c}
  }

  def decrypt(crypt: String, key: Key[(BigInt, BigInt)] ): String = 
  {
    val (n, d) = key.getPrivate
    val chunks = crypt.grouped(PQ_LENGTH).toArray 

    val decrypted =  chunks.map(BigInt.apply(_).modPow(d,n).toString).foldLeft(""){(s,c)=>s+c}
    PKCS1IntToString(BigInt.apply(decrypted))
  }

}

val a = new RsaKey(12)
val b = "uhuhuhuhuhuhuhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnuhuhuhuhuhuhuhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnop5uhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnop5uhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnop5uhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnop5uhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnop5uhuhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjbbbbbbbbbbbbbbbbbbbbbbjjjjjjjjjjtestcoucou12345123451234512345123451234512345123451234512345123451234bcdefghijklmnop5"
val rsa = new RSA

val f = rsa.encrypt(b, a)
println(f)
println(rsa.decrypt(f, a))
