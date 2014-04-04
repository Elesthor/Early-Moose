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

class ElGamalKey[E: Manifest](seed: Int, group: Group[E]) extends Key[(BigInt, E)] // public : (_, g^x); private : (x, _)
{
  def generate() =
  {
    val randomizer = new util.Random(seed)
    val x = randomizer.nextBigInt(group.order-2)+1
    (group.exp(group.generator, x), x)
  }
  
  var (h, x) = generate()
  def getPublic  = (0, h)
  def getPrivate = (x, group.unit)
}

class ElGamal[E](group: Group[E], seed: Int) extends CryptoSystem [(BigInt, E)]
{
// UTILITIES :
  // convert e to a byte
  def eToByte(e: E): Byte =
  {
    var el = group.generator
    for(i <- -127 to 128)
    {
      if(el == e)
        return i.toByte
      el = group.times(el, group.generator)
    }
    throw new java.lang.RuntimeException("E not found :(" + new String(group.eToBytes(e)))
  }
  
  // convert a byte to an e
  def eFromByte(c: Byte): E =
    group.exp(group.generator, c+128)
  
  // encode a string in a way to decode a concatenation injectively
  def injectiveString(s: Array[Byte]): Array[Byte] =
    networkToArray(s.length.toString) ++ Array('#'.toByte) ++ s
  // and decode it : return the head and the tail
  def getString(s: Array[Byte]): (Array[Byte], Array[Byte]) =
  {
    val p = s.indexOf('#') // if not found, substring will throw an exception, and we want it TODO slice ?
    val len = arrayToNetwork(s.slice(0, p)).toInt
    //println(new String(s.slice(p+1, p+len+1)))
    //println(new String(s.slice(p+len+1, s.length)))
    (s.slice(p+1, p+len+1), s.slice(p+len+1, s.length))
  }
  // encode a pair of E in a way to decode a concatenation injectively
  def injectiveCode(c: (E, E)): Array[Byte] =
  {
    injectiveString(group.eToBytes(c._1)) ++ injectiveString(group.eToBytes(c._2))
  }
  // and decode it : return (c1, c2) and the tail
  def getCode(from: Array[Byte]): (E, E, Array[Byte]) =
  {
    val (c1, tmp) = getString(from)
    val (c2, next) = getString(tmp)
    (group.eFromBytes(c1), group.eFromBytes(c2), next)
  }
  val randomizer = new util.Random(seed)
  def _encrypt (msg: Array[Byte], key: (BigInt, E)): Array[Byte]  =
  {
    def encryptE(m: E): (E, E) =
    {
      val y = randomizer.nextBigInt(group.order-2)+1
      val s = group.exp(key._2, y)
      val c2 = group.times(m, s)
      val c1 = group.exp(group.generator, y)
      (c1, c2)
    }
    msg.foldLeft(Array[Byte]()){(s,c) => s ++ injectiveCode(encryptE(eFromByte(c)))}
  }

  def _decrypt (msg: Array[Byte], key : (BigInt, E)): Array[Byte] =
  {
    def decryptE(c1: E, c2: E) : E =
    {
      val s_inv = group.exp(c1, group.order-key._1)
      val m = group.times(c2, s_inv)
      m
    }
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


object TestElGamal
{
  def main(args: Array[String]): Unit =
  {
    val grp = new Zk(1009)
    val key = new ElGamalKey(0, grp)
    val gen = new ElGamal(grp, 10)
    val msg = "asalut les coupains :D !▤"
    println(msg)
    val cypher = gen.encrypt(msg,key)
    println(cypher)
    println(gen.decrypt(cypher, key))
  }
}

