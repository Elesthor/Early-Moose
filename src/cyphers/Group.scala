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

trait Group[E]
{
  val unit: E
  val generator: E
  val order: BigInt
  def times (e1: E, e2: E) : E
  def expContinuation(e: E, n: BigInt) : E =
  {
    def rec(el: E, n: BigInt, f: (E=>E)) : E =
    {
      if(n==1)
        f(el)
      else if(n % 2 == 0)
        rec(times(el, el), n/2, f)
      else
        rec(times(el, el), n/2, {x => f(times(el, x))})
    }
    rec(e, n, {x=>x})
  }
  def exp (e: E, n: BigInt) : E =
  {
    expContinuation(e, n)
    /*if(n == 1)
      e
    else if(n % 2 == 0)
      exp(times(e, e), n/2)
    else
      times(exp(times(e, e), n/2), e)*/
  }
  def eToString(e: E): String
  def eFromString(s: String): E
}

class Zk(k: BigInt) extends Group[BigInt]
{
  val unit = BigInt(0)
  val generator = BigInt(1) // TODO : autre chose
  val order = k
  def times (e1: BigInt, e2: BigInt) : BigInt =
  {
    (e1+e2)%k
  }
  def eToString(e: BigInt): String =
    e.toString // TODO : faire des octets, plutot que 0-9*
  def eFromString(s: String): BigInt =
    BigInt(s)
}

class Zp(p: BigInt) extends Group[BigInt]
{
  val unit = BigInt(1)
  val generator = BigInt(2) // TODO
  val order = p-1
  def times (e1: BigInt, e2: BigInt) : BigInt =
  {
    (e1*e2)%p
  }
  def eToString(e: BigInt): String =
    e.toString
  def eFromString(s: String): BigInt =
    BigInt(s)
}

// TODO : courbes elliptiques

