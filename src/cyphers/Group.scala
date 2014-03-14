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
  def times (e1: E, e2: E) : E
  def exp (e: E, n: Int) : E =
  {
    if(n % 2 == 0)
      exp(times(e, e), n/2)
    else if(n == 1)
      e
    else
      times(exp(times(e, e), n/2), e)
  }
}

class Zk(k: Int) extends Group[Int]
{
  val unit = 0
  val generator = 1
  def times (e1: Int, e2: Int) : Int =
  {
    (e1+e2)%k
  }
}

class Zp(p: Int) extends Group[Int]
{
  val unit = 1
  val generator = 2
  def times (e1: Int, e2: Int) : Int =
  {
    (e1*e2)%p
  }
}

// TODO : courbe elliptique

