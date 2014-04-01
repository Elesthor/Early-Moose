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
  def exp (e: E, n: BigInt) : E =
  {
    if(n == 0)
      generator
    if(n == 1)
      e
    else if(n % 2 == 0)
      exp(times(e, e), n/2)
    else
      times(exp(times(e, e), n/2), e)
  }
  def eToBytes(e: E): Array[Byte]
  def eFromBytes(s: Array[Byte]): E
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
  def eToBytes(e: BigInt): Array[Byte] =
    e.toByteArray
  def eFromBytes(s: Array[Byte]): BigInt =
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
  def eToBytes(e: BigInt): Array[Byte] =
    e.toByteArray
  def eFromBytes(s: Array[Byte]): BigInt =
    BigInt(s)
}

// TODO : courbes elliptiques

