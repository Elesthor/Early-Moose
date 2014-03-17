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

// TODO : on peut retrouver publicKey en retrouvant l'indice de g^publicKey ???
class ElGamalKey[E: Manifest](seed: Int, group: Group[E]) extends Key[(Int, Int, E)] // public : (|G|,_, g^x); private : (|G|, x, _)
{
  def generate() =
  {
    // generation of the whole group
    var els = new Array[E](0)
    var g = group.generator
    var e = g
    do
    {
      els = els :+ e
      e = group.times(g, e)
    } while(e != g);
    
    // key
    val randomizer = new util.Random(seed)
    val x = randomizer.nextInt(els.length-1)+1
    
    (els, els.length, group.exp(g, x), x)
  }
  
  var (elements, size, h, x) = generate()
  def getPublic  = (size, 0, h)
  def getPrivate = (size, x, group.unit)
}
type E = Int
class ElGamal/*[E]*/(group: Group[E]) extends CryptoSystem [(Int, Int, E)]
{
  def encrypt (msg: String, key: Key[(Int, Int, E)]): String  =
  {
    val k = key.getPublic
    def encryptChar(c: Char) : String =
    {
      val y = util.Random.nextInt(k._1-1)+1
      val s = group.exp(k._3, y)
      val m = c // TODO : on est sensé choisir un élément du groupe...
      val c2 = group.times(m, s)
      val c1 = group.exp(group.generator, y)
      val cypher = (c1, c2)
      cypher.toString
}
    msg.toArray.foldLeft(""){(s,c) => s + encryptChar(c)}
  }

  def decrypt (msg: String, key : Key[(Int, Int, E)]): String =
  {
    val k = key.getPrivate
    def decryptChar(text: String) : (Char, String) =
    {
      val regex = "\\(([0-9]+),([0-9]+)\\)(.*)".r
      val Some(res) = regex findFirstMatchIn text
      val c1 = res.group(1).toInt // TODO
      val c2 = res.group(2).toInt
      val s_inv = group.exp(c1, k._1-k._2)
      val m = group.times(c2, s_inv)
      (m.toChar, res.group(3))
    }
    var from = msg
    var to = ""
    while(from != "")
    {
      var (c, next) = decryptChar(from)
      to = to + c
      from = next
    }
    to
  }
}

val grp = new Zk(1009)
val key = new ElGamalKey(0, grp)
val gen = new ElGamal(grp)
val msg = "coucou"
val cypher = gen.encrypt(msg,key)
println(msg.toArray.foldLeft(""){(s,c) => s+','+c.toInt})
println(cypher)
println(gen.decrypt(cypher, key).toArray.foldLeft(""){(s,c) => s+','+c.toInt})


