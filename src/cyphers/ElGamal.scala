////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [ElGamal.scala]                                                //
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

// RandomWithBigInt
import perso.utils.BigIntUtils._
// injectivePair and getPair
import perso.utils.NetworkTools._


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                ElGamal Key                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

// public  key : (_, g^x, order)
// order to be able to make an attack (we suppose we know the group)
// private key : (x, _, _)
class ElGamalKey[E](group: Group[E], seed: Long) extends Key[(BigInt, E, BigInt)]
{
  val (h, x) = 
  {
    val x = new util.Random(seed).nextBigInt(group.order-2)+1
    (group.exp(group.generator, x), x)
  }
  def getPublic  = (0, h, group.order)
  def getPrivate = (x, group.unit, group.order)

  def getString(k: (BigInt, E, BigInt)): String =
    // not the same order !
    k._1.toString + "," + k._3.toString + "," +
    new String(group.eToBytes(k._2), java.nio.charset.Charset.forName("ISO-8859-1"))
    
  def fromString(s: String): (BigInt, E, BigInt) =
  {
    val d = s.split(",", 3)
    assert(d.length == 3)
    (BigInt(d(0)),
     group.eFromBytes(d(2).getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))),
     BigInt(d(1)))
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          ElGamal implementation                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


// group.order must be >= 256
class ElGamal[E](group: Group[E]) extends CryptoSystem [(BigInt, E, BigInt)]
{
  // UTILITIES :
  // convert e to a byte (discrete logarithm on a little range)
  def eToByte(e: E): Byte =
  {
    var el = group.generator
    for(i <- -127 to 128)
    {
      if(el == e)
        return i.toByte
      el = group.times(el, group.generator)
    }
    throw new RuntimeException("E not found :( " + new String(group.eToBytes(e)))
  }

  // convert a byte to an e
  def eFromByte(c: Byte): E = group.exp(group.generator, c+128)

  // encode a pair of E in a way to decode a concatenation injectively
  def injectiveCode(c: (E, E)): Array[Byte] =
    injectivePair(group.eToBytes(c._1), group.eToBytes(c._2))

  // and decode it : return c1, c2 and the tail
  def getCode(m: Array[Byte]): (E, E, Array[Byte]) =
  {
    val c = getPair(m)
    (group.eFromBytes(c._1), group.eFromBytes(c._2), c._3)
  }
  
  def _encrypt (msg: Array[Byte], key: (BigInt, E, BigInt), seed: Long): Array[Byte]  =
  {
    val randomizer = new util.Random(seed)
    def encryptE(m: E): (E, E) =
    {
      val y = randomizer.nextBigInt(group.order-2)+1
      val s = group.exp(key._2, y)
      (group.exp(group.generator, y), group.times(m, s))
    }
    msg.foldLeft(Array[Byte]()){(s,c) => s ++ injectiveCode(encryptE(eFromByte(c)))}
  }

  def _decrypt (msg: Array[Byte], key : (BigInt, E, BigInt)): Array[Byte] =
  {
    def decryptE(c1: E, c2: E) : E =
      group.times(c2, group.inv(group.exp(c1, key._1)))
    var from = msg
    var to = Array[Byte]()
    while(!from.isEmpty)
    {
      val (c1, c2, next) = getCode(from)
      to = to :+ eToByte(decryptE(c1, c2))
      from = next
    }
    to
  }
}


class EncapsulatedElGamalEc extends EncapsulatedCrypto
{
  val field = new Zpf(BigInt("20988936657440586486151264256610222593863921"))
  val grp = new Elliptic[BigInt](
    field, 4, (2, 2), BigInt("100000000000000000000")) // TODO mettre une bonne courbe
  type T = (BigInt, Option[(BigInt,BigInt)], BigInt)
  val crypto = new ElGamal(grp)
  def makeKey(seed: Long) =
  {
    val key = new ElGamalKey(grp, seed)
    (key, key.getString(key.getPublic))
  }
  def encrypt(msg: String, key: T, seed: Long) = crypto.encrypt(msg, key, seed)
  def decrypt(msg: String, key: T) = crypto.decrypt(msg, key)
}
class EncapsulatedElGamalZk(k: BigInt) extends EncapsulatedCrypto
{
  val grp = new Zk(k)
  type T = (BigInt, BigInt, BigInt)
  val crypto = new ElGamal(grp)
  def makeKey(seed: Long) =
  {
    val key = new ElGamalKey(grp, seed)
    (key, key.getString(key.getPublic))
  }
  def encrypt(msg: String, key: T, seed: Long) = crypto.encrypt(msg, key, seed)
  def decrypt(msg: String, key: T) = crypto.decrypt(msg, key)
}
class EncapsulatedElGamalZp(p: BigInt, g: BigInt) extends EncapsulatedCrypto
{
  val grp = new Zp(p, g)
  type T = (BigInt, BigInt, BigInt)
  val crypto = new ElGamal(grp)
  def makeKey(seed: Long) =
  {
    val key = new ElGamalKey(grp, seed)
    (key, key.getString(key.getPublic))
  }
  def encrypt(msg: String, key: T, seed: Long) = crypto.encrypt(msg, key, seed)
  def decrypt(msg: String, key: T) = crypto.decrypt(msg, key)
}
class ElGamalOpponentZk extends Opponent
{
  def openEnc(crypt: Array[Byte], infos : Term):Array[Byte] =
  {
    val (_, h, k) = infos match
    {
      case TRaw(pubkey) =>
        // the key decoding don't depend on order :
        new ElGamalKey(new Zk(BigInt(1000)), 0).fromString(pubkey)
    }
    val group = new Zk(k)
    val crypto = new ElGamal(group)
    
    // on a h = gx mod k, on cherche x :
    val x = (group.generator.modInverse(k)*h)%k
    
    def decryptE(c1: BigInt, c2: BigInt) : BigInt =
      group.times(c2, group.inv(group.exp(c1, x)))
    
    var from = crypt
    var to = Array[Byte]()
    while(!from.isEmpty)
    {
      val (c1, c2, next) = crypto.getCode(from)
      to = to :+ crypto.eToByte(decryptE(c1, c2))
      from = next
    }
    to
  }
}

