////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [RandomWithBigInt.scala]                                       //
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
//
// class with two utilities :
// randomToRandomWithBigInt to extends implicitly util.Random
// to provide random bigint
// and square root

package perso.utils
import scala.BigInt

object BigIntUtils
{
  implicit def randomToRandomWithBigInt(rnd: util.Random):RandomWithBigInt = 
    new RandomWithBigInt(rnd)

  class RandomWithBigInt(rnd: util.Random)
  {
    // return a random BigInt between 0 (inclusive) and max (exclusive)
    def nextBigInt(max: BigInt) : BigInt =
    {
      val size = max.bitLength
      var r = BigInt(0)
      do 
      {
        r = BigInt(size, rnd);
      } while (r >= max);
      r
    }
  }
  
  // floor of square root
  def sqrt(n: BigInt): BigInt =
  {
    var a = BigInt(1)
    var b = (n >> 5) + 8
    while(b >= a)
    {
      val mid = (a + b) >> 1
      if(mid*mid > n) b = mid - 1
      else a = mid + 1
    }
    a - 1
  }
}

