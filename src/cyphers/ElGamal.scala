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

import scala.BigInt

class ElGamalKey[E: Manifest](seed: Int, group: Group[E]) extends Key[(BigInt, E)] // public : (_, g^x); private : (x, _)
{
  // return a random BigInt between 0 (inclusive) and max (exclusive)
  def randomBigInt(max: BigInt, rnd: util.Random) : BigInt = // TODO : étendre Random
  {
    val size = max.bitLength
    var r = BigInt(0)
    do {
      r = BigInt(size, rnd);
    } while (r >= max);
    r
  }
  
  def generate() =
  {
    val randomizer = new util.Random(seed)
    val x = randomBigInt(group.order-1, randomizer) // TODO : -1 ? on veut pas unit, on veut le générateur ?
    (group.exp(group.generator, x), x)
  }
  
  var (h, x) = generate()
  def getPublic  = (0, h)
  def getPrivate = (x, group.unit)
}

class ElGamal[E](group: Group[E]) extends CryptoSystem [(BigInt, E)]
{
  // return a random BigInt between 0 (inclusive) and max (exclusive)
  def randomBigInt(max: BigInt, rnd: util.Random) : BigInt = // TODO : étendre Random
  {
    val size = max.bitLength
    var r = BigInt(0)
    do {
      r = BigInt(size, rnd);
    } while (r >= max);
    r
  }
  
  def eToByte(e: E): Byte =
  {
    var el = group.generator
    for(i <- 1 to 256)
    {
      if(el == e)
        return (i-128).toByte
      el = group.times(el, group.generator)
    }
    throw new java.lang.RuntimeException("E not found :(")
  }
  def eFromByte(c: Byte): E =
  {
    group.exp(group.generator, c+128)
  }
  
  def injectiveString(s: String): String =
  {
    s.length.toString + '#' + s // TODO : etre transparent à l'encodage, car là la taille de la string en dépend (lors de l'interpratation des octets depuis le réseau)... bytes plutot que char
  }
  
  def getString(s: String): (String, String) =
  {
    val p = s.indexOf('#') // if not found, substring will throw an exception, and we want it
    val len = s.substring(0, p).toInt
    (s.substring(p+1, p+len+1), s.substring(p+len+1))
  }
  def injectiveCode(c: (E, E)): String =
  {
    injectiveString(group.eToString(c._1))+injectiveString(group.eToString(c._2))
  }
  def getCode(from: String): (E, E, String) =
  {
    val (c1, tmp) = getString(from)
    val (c2, next) = getString(tmp)
    (group.eFromString(c1), group.eFromString(c2), next)
  }
  
  def encrypt (msg: String, key: Key[(BigInt, E)]): String  =
  {
    val randomizer = new util.Random
    val k = key.getPublic
    def encryptE(m: E): (E, E) =
    {
      val y = randomBigInt(group.order-1, randomizer) // TODO : -1 ? on veut pas unit, on veut le générateur ?
      val s = group.exp(k._2, y)
      val c2 = group.times(m, s)
      val c1 = group.exp(group.generator, y)
      (c1, c2)
    }
    msg.getBytes.foldLeft(""){(s,c) => s + injectiveCode(encryptE(eFromByte(c)))}
  }

  def decrypt (msg: String, key : Key[(BigInt, E)]): String =
  {
    val k = key.getPrivate
    def decryptE(c1: E, c2: E) : E =
    {
      val s_inv = group.exp(c1, group.order-k._1)
      val m = group.times(c2, s_inv)
      m
    }
    var from = msg
    var to = Array[Byte]()
    while(from != "")
    {
      val (c1, c2, next) = getCode(from)
      to :+ eToByte(decryptE(c1, c2))
      from = next
    }
    new String(to)
  }
}


object Test
{
  def main(args: Array[String]): Unit =
  {
    val grp = new Zk(1009)
    val key = new ElGamalKey(0, grp)
    val gen = new ElGamal(grp)
    val msg = "salut les coupains :D ! ▤"
    val cypher = gen.encrypt(msg,key)
    println(msg)//.toArray.foldLeft(""){(s,c) => s+','+c.toInt})
    //println(cypher)
    println(gen.decrypt(cypher, key))//.toArray.foldLeft(""){(s,c) => s+','+c.toInt})
  }
}
