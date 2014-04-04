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

package perso.utils
import scala.BigInt

object BigIntUtils
{
  implicit def randomToRandomWithBigInt(rnd: util.Random):RandomWithBigInt = new RandomWithBigInt(rnd)

  class RandomWithBigInt(rnd: util.Random)
  {
    // return a random BigInt between 0 (inclusive) and max (exclusive)
    def nextBigInt(max: BigInt) : BigInt =
    {
      val size = max.bitLength
      var r = BigInt(0)
      do {
        r = BigInt(size, rnd);
      } while (r >= max);
      r
    }
  }
}

