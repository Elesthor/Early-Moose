////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Group.scala]                                                  //
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

// injectivePair and getPair
import perso.utils.NetworkTools._
import perso.utils.BigIntUtils.sqrt

// cyclic groups
abstract class Group[E]
{
  val unit: E
  val generator: E
  val order: BigInt // could be an underestimation of the order
  def times (e1: E, e2: E): E
  def inv(e: E) : E
  def eToBytes(e: E): Array[Byte]
  def eFromBytes(s: Array[Byte]): E
  
  def div(e1: E, e2: E): E = times(e1, inv(e2))
  def exp (e: E, n: BigInt): E =
  {
    assert(n>=0)
    if(n == 0)
      generator
    else if(n == 1)
      e
    else if(n % 2 == 0)
      exp(times(e, e), n/2)
    else
      times(exp(times(e, e), n/2), e)
  }
}

// additive group Zk
class Zk(k: BigInt) extends Group[BigInt]
{
  val unit = BigInt(0)
  val generator = k-1
  val order = k
  def times (e1: BigInt, e2: BigInt): BigInt = (e1+e2)%k
  def inv(e: BigInt): BigInt = k-e
  def eToBytes(e: BigInt): Array[Byte] = e.toByteArray
  def eFromBytes(s: Array[Byte]): BigInt = BigInt(s)
}

// multiplying group Zp (with p a prime number), g is a generator
class Zp(p: BigInt, g:BigInt) extends Group[BigInt]
{
  val unit = BigInt(1)
  val generator = g
  val order = p-1
  def times (e1: BigInt, e2: BigInt): BigInt = (e1*e2)%p
  def inv(e: BigInt): BigInt = e.modPow(p-2, p)
  def eToBytes(e: BigInt): Array[Byte] = e.toByteArray
  def eFromBytes(s: Array[Byte]): BigInt = BigInt(s)
}

// finite field
abstract class Field[E]
{
  val group:Group[E]
  def one:E
  def times(e1: E, e2: E): E
  def inv(e: E): E
  
  val zero = group.unit
  def plus(e1: E, e2: E) = group.times(e1, e2)
  def minus(e1: E, e2: E) = group.div(e1, e2)
  def ktimes(e: E, k: BigInt) = group.exp(e, k)
  def div(e1: E, e2: E) = times(e1, inv(e2))
}

// field Fp
class Zpf(p: BigInt) extends { val group = new Zk(p) } with Field[BigInt]
{
  def one = BigInt(1)
  def times(e1: BigInt, e2: BigInt) = (e1*e2)%p
  def inv(e: BigInt) = e.modPow(p-2, p)
}

// Notes :
// None is inf
// the curve is y² = x³ + ax² + b (b given by g, the generator)
class Elliptic[K](f: Field[K], a: K, g: (K, K), o:BigInt) extends Group[Option[(K, K)]]
{
  val unit = None
  val generator = Some(g)
  val order = o // f.group.order-1-2*sqrt(f.group.order)
  def times(e1: Option[(K, K)], e2: Option[(K, K)]): Option[(K, K)] =
  {
    // different cases of addition :
    (e1, e2) match
    {
      case (None, e) => e
      case (e, None) => e
      case (Some(p), Some(q)) =>
        if(p._1 != q._1)
        {
          val s = f.div(f.minus(p._2, q._2), f.minus(p._1, q._1))
          val t = f.div(f.minus(f.times(q._2, p._1), f.times(p._2, q._1)), f.minus(p._1, q._1))
          val x = f.minus(f.minus(f.times(s, s), p._1), q._1)
          Some((x, f.minus(f.minus(f.zero, t), f.times(s, x))))
        }
        else if(p._2 != q._2)
          None
        else if(p._2 != f.zero)
        {
          val s = f.div(f.plus(f.ktimes(f.times(p._1, p._1), 3), a), f.ktimes(p._2, 2))
          val x = f.minus(f.minus(f.times(s, s), p._1), q._1)
          Some((x, f.minus(f.times(s, f.minus(p._1, x)), p._2)))
        }
        else
          None
    }
  }
  def inv(e: Option[(K, K)]) =
  {
    e match
    {
      case None => None
      case Some((x, y)) => Some((x, f.minus(f.zero, y)))
    }
  }
  def eToBytes(e: Option[(K, K)]) =
  {
    e match
    {
      case None => Array[Byte]()
      case Some((x, y)) =>
        injectivePair((f.group.eToBytes(x), f.group.eToBytes(y)))
    }
  }
  def eFromBytes(m: Array[Byte]) =
  {
    if(m.isEmpty)
      None
    else
    {
      val c = getPair(m)
      Some(f.group.eFromBytes(c._1), f.group.eFromBytes(c._2))
    }
  }
}

