////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Vigenere.scala]                                               //
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Vigenere Key                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class VigenereKey(seed: Int) extends Key[Array[Byte]]
{
  def generate() : Array[Byte] =
  {
    // random Array[Byte] of random size
    val randomizer = new util.Random(seed)
    // nextString return string of any character (chinese...), we cast them to bytes
    randomizer.nextString(randomizer.nextInt() % 64 + 64).toCharArray.map(_.toByte)
  }
  
  def getPublic  = generate()
  def getPrivate = getPublic.map({x => (-x).toByte})
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            Vigenere Implementation                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Vigenere extends CryptoSystem [Array[Byte]]
{
  def _encrypt (msg: Array[Byte], key: Array[Byte], _seed:Int = 0): Array[Byte] =
    // folding : s._1 is the array, and s._2 is a counter
    msg.foldLeft(Array[Byte](),0){(s, c) => (s._1 :+ (c+key(s._2 % key.length)).toByte, s._2+1)}._1

  def _decrypt (msg: Array[Byte], key : Array[Byte]): Array[Byte] =
    _encrypt (msg, key)
}

object TestVigenere
{
  def main(args: Array[String]): Unit =
  {
    val key = new VigenereKey(0)
    val gen = new Vigenere
    val msg = "asalut les coupains :D !▤"
    println(msg)
    val cypher = gen.encrypt(msg,key)
    println(cypher)
    println(gen.decrypt(cypher, key))
  }
}

