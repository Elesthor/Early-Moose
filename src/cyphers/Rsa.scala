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
class RsaKey (seed: Int) extends Key [(BigInt, BigInt)]
{ 
  val PQ_LENGTH = 2048
  def generate() =
  {
    val randomizer = new util.Random(seed)
    val p = BigInt.apply(PQ_LENGTH, 10000, randomizer)
    val q = BigInt.apply(PQ_LENGTH, 10000, randomizer)
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            RSA Implementation                              //  
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class RSA extends CryptoSystem [(BigInt, BigInt)]
{

  // Return a padded representation of the byte at len digit.
  def padByte(s: String , len: Int): String = 
  {
    "0"*(len-s.toString.length)+s.toString
  }
  
  def padByteMod(s:String, n: Int): String = 
  {
    padByte(s, ((s.length/m+1)*m-s.length))
  }
  def PKCS1StringToInt(msg: Array[Byte]): String  = 
  {
    (msg.map({x => padByte((x+0x80).toString, 3)})).foldLeft(""){(s,c)=>s+c}
  }

  def PKCS1IntToString(nb: String): Array[Byte] =
  {
     (BigInt(nb).toString.grouped(3).toArray.map({x=>(x.toInt-0x80).toByte}))
  }

  def _encrypt(msg: Array[Byte], key:(BigInt, BigInt)): Array[Byte] = 
  {
    val (n, e) = key
    // Split the input string into blocks.
    val chunks = PKCS1StringToInt(msg).grouped(513).toArray  
    // Crypt each blocks and concat them
    chunks.map({x => padByte(BigInt.apply(x).modPow(e,n).toString,2*n.bitLength/3) }).foldLeft(""){(s,c)=>s+c}.getBytes

  }

  def _decrypt(crypt: Array[Byte], key: (BigInt, BigInt) ): Array[Byte] = 
  {
    val (n, d) = key   
    val chunks = (new String(crypt)).grouped(2*n.bitLength/3).toArray
    val decrypted =  chunks.map({x => padByteMod(BigInt.apply(x).modPow(d,n).toString,3)}).foldLeft(""){(s,c)=>s+c}
    PKCS1IntToString(decrypted)
  }
}

