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

// public  key : (_, g^x)
// private key : (x, _)
class ElGamalKey[E](group: Group[E], seed: Int) extends Key[(BigInt, E)]
{
  val (h, x) = 
  {
    val x = new util.Random(seed).nextBigInt(group.order-2)+1
    (group.exp(group.generator, x), x)
  }
  def getPublic  = (0, h)
  def getPrivate = (x, group.unit)
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          ElGamal implementation                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


// group.order must be >= 256
class ElGamal[E](group: Group[E]) extends CryptoSystem [(BigInt, E)]
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
  
  def _encrypt (msg: Array[Byte], key: (BigInt, E), seed: Int): Array[Byte]  =
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

  def _decrypt (msg: Array[Byte], key : (BigInt, E)): Array[Byte] =
  {
    def decryptE(c1: E, c2: E) : E =
      //group.times(c2, group.exp(c1, group.order-key._1)) // TODO raconter ça dans le rapport, et supprimer la ligne
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


object TestElGamal
{
  def main(args: Array[String]): Unit =
  {
    //val grp = new Zk(1009)
    val f = new Zpf(2147483647)
    val grp = new Elliptic[BigInt](f, 1, (1, 3))
    
    val key = new ElGamalKey(grp,5)
    val gen = new ElGamal(grp)
    println(grp.order)
    val msg = "asalut les coupains :D !▤"
    println(msg)
    val cypher = gen.encrypt(msg,key,15)
    println(cypher)
    println(gen.decrypt(cypher, key))
  }
}

