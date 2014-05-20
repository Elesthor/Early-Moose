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

import scala.BigInt
import scala.math

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                              Key Generator                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


class RsaKey (seed: Long, pqLength: Int = 1024) extends Key [(BigInt, BigInt)]
{ 
  val (n,d,e) = 
  {
    val randomizer = new util.Random(seed)
    val p = BigInt(pqLength, 10000, randomizer)
    val q = BigInt(pqLength, 10000, randomizer)
    val n1 = p*q
    val indEulerN = (p-1)*(q-1)
    
    var e1 = BigInt(indEulerN.bitLength-1, randomizer)
    while (e1.gcd(indEulerN) !=  BigInt(1))
    {
      e1 = BigInt(indEulerN.bitLength-1, randomizer)
    }
    val d1 = e1.modInverse(indEulerN)
    (n1,d1,e1)
  }
  
  def getPublic = (n,e)
  def getPrivate = (n,d)

  def getString(k: (BigInt, BigInt)): String =
    k._1.toString + "," + k._2.toString
  def fromString(s: String): (BigInt, BigInt) =
  {
    val d = s.split(",")
    assert(d.length == 2)
    (BigInt(d(0)), BigInt(d(1)))
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            RSA Implementation                              //  
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Rsa extends CryptoSystem [(BigInt, BigInt)]
{

  // Return a padded representation of the byte at len digit.
  def padByte(s: String , len: Int): String = 
  {
    "0"*(len-s.toString.length)+s.toString
  }
  
  def padByteMod(s:String, n: Int): String = 
  {
    padByte(s, ((s.length/n+1)*n-s.length))
  }
  def PKCS1StringToInt(msg: Array[Byte]): String  = 
  {
    (msg.map({x => padByte((x+0x80).toString, 3)})).foldLeft(""){(s,c)=>s+c}
  }
  
  def PKCS1StringToInt(msg: Array[Byte], seed: Long): String  = 
  {
    val randomizer = new util.Random(seed)
    var rando = BigInt(0x80, randomizer)
    if  (rando%10 == 0) rando += 1
    val randomness = "0000"+ rando.toString + "0"*0x10
    randomness+ (msg.map({x => padByte((x+0x80).toString, 3)})).foldLeft(""){(s,c)=>s+c}
  }

  def PKCS1IntToString(nb: String): Array[Byte] =
  {
     (BigInt(nb).toString.grouped(3).toArray.map({x=>(x.toInt-0x80).toByte}))
  }

  def PKCS1IntToString2(nb: String): Array[Byte] =
  {
     (BigInt(nb).toString).split("""0000000000000000""")(1).grouped(3).toArray.map(
                                                          {x=>(x.toInt-0x80).toByte})
  }

  def _encrypt(msg: Array[Byte], key:(BigInt, BigInt), seed: Long = 0): Array[Byte] = 
  {
    val (n, e) = key
    // Split the input string into blocks.
    val chunks = PKCS1StringToInt(msg, seed).grouped(n.bitLength/4).toArray  
    // Crypt each blocks and concat them
    chunks.map({x => padByte(BigInt(x).modPow(e,n).toString, n.bitLength/3) }).foldLeft(""){(s,c)=>s+c}.getBytes
  }

  def _decrypt(crypt: Array[Byte], key: (BigInt, BigInt) ): Array[Byte] = 
  {
    val (n, d) = key   
    val chunks = (new String(crypt)).grouped(n.bitLength/3).toArray
    val decrypted =  chunks.map({x => padByteMod(BigInt(x).modPow(d,n).toString,3)}).foldLeft(""){(s,c)=>s+c}
    PKCS1IntToString2(decrypted)
  }
}

class EncapsulatedRsa(pqLength:Int) extends EncapsulatedCrypto
{
  type T = (BigInt, BigInt)
  val crypto = new Rsa()
  def makeKey(seed: Long) =
  {
    val key = new RsaKey(seed, pqLength)
    (key, key.getString(key.getPublic))
  }
  def encrypt(msg: String, key: T, seed: Long) = crypto.encrypt(msg, key, seed)
  def decrypt(msg: String, key: T) = crypto.decrypt(msg, key)
}
/*
val r = new Rsa()
val k = new RsaKey(1)
val m = "coucou"
val c = r.encrypt(m, k.getPublic, 5)
println(m)
println(c)
println(r.decrypt(c, k.getPrivate))
*/
