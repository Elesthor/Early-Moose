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


class RsaKey (seed: Int, pqLength: Int = 2048) extends Key [(BigInt, BigInt)]
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
    padByte(s, ((s.length/n+1)*n-s.length))
  }
  def PKCS1StringToInt(msg: Array[Byte]): String  = 
  {
    (msg.map({x => padByte((x+0x80).toString, 3)})).foldLeft(""){(s,c)=>s+c}
  }
  
  def PKCS1StringToInt(msg: Array[Byte], seed: Int): String  = 
  {
    val randomizer = new util.Random(seed)
    val randomness = "0000"+ BigInt(0x80, randomizer).toString + "01"
    randomness+ (msg.map({x => padByte((x+0x80).toString, 3)})).foldLeft(""){(s,c)=>s+c}
  }

  def PKCS1IntToString(nb: String): Array[Byte] =
  {
     (BigInt(nb).toString.grouped(3).toArray.map({x=>(x.toInt-0x80).toByte}))
  }

  def PKCS1IntToString2(nb: String): Array[Byte] =
  {
     (BigInt(nb).toString.drop(0x86).grouped(3).toArray.map({x=>(x.toInt-0x80).toByte}))
  }

  def _encrypt(msg: Array[Byte], key:(BigInt, BigInt), _seed: Int = 0): Array[Byte] = 
  {
    val (n, e) = key
    // Split the input string into blocks.
    val chunks = PKCS1StringToInt(msg, _seed).grouped(n.bitLength/8).toArray  
    // Crypt each blocks and concat them
    chunks.map({x => padByte(BigInt(x).modPow(e,n).toString,2*n.bitLength/3) }).foldLeft(""){(s,c)=>s+c}.getBytes

  }

  def _decrypt(crypt: Array[Byte], key: (BigInt, BigInt) ): Array[Byte] = 
  {
    val (n, d) = key   
    val chunks = (new String(crypt)).grouped(2*n.bitLength/3).toArray
    val decrypted =  chunks.map({x => padByteMod(BigInt(x).modPow(d,n).toString,3)}).foldLeft(""){(s,c)=>s+c}
    PKCS1IntToString2(decrypted)
  }
}
val test = new RSA()
val p = new RsaKey(12)
val t = "couccoicouccocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiucocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiuocuocucooucoucocuocoucocucocuocucouocucocucocucoucocuocucoucocuocuciodhfijbgerihbribihpbipibhipbiiubiupbbiuiubiububiiuu"
println( test.encrypt(t, p) )
println( test.decrypt(test.encrypt(t,p),p) )
