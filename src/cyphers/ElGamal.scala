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
class ElGamalKey[E: Manifest](seed: Int, group: Group[E]) extends Key[(Array[E], Int, E)] // Key : (G, |G|, g^publicKey)
{
  var (elements, size, h, x) = generate()
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
  generate()
  
  def getPublic  = (elements, size, h)
  def getPrivate = (elements, x, group.unit) // only x count. Plus maintenant en fait j'ai besoin de la taille de elements pour calculer s^-1
}

class ElGamal[E](group: Group[E]) extends CryptoSystem [(Array[E], Int, E)]
{
  def encrypt (msg: String, key: Key[(Array[E], Int, E)]): String  =
  {
    val k = key.getPublic
    val y = util.Random.nextInt(k._2-1)+1
    val s = group.exp(group.generator, y)
    val m = group.exp(group.generator, 2)// TODO : on est sensé choisir un élément du groupe...
    val c2 = group.times(m, s)
    val cypher = (group.exp(group.generator, y), c2)
    cypher.toString
  }

  def decrypt (msg: String, key : Key[(Array[E], Int, E)]): String =
  {
    val k = key.getPrivate
    val c1 = group.generator // TODO
    val c2 = group.generator // TODO
    val s = group.exp(c1, k._2)
    val s_inv = group.exp(s, k._1.length-1)
    val m = group.times(c2, s_inv)
    // puis retrouver le message correspondant
    ""
  }
}


