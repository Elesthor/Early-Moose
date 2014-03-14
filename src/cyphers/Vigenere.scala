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

class VigenereKey(seed: Int) extends Key[Array[Char]]
{
  def generate() : Array[Char] =
  {
    // random Array[Char] of random size
    val randomizer = new util.Random(seed)
    randomizer.nextString(randomizer.nextInt() % 64 + 64).toArray
  }
  
  def getPublic  = generate()
  def getPrivate = getPublic.map({x => (-x).toChar})
}

class Vigenere extends GenericCypher[Array[Char]]
{
  // we have to use a charset for convertion between Array[Char] and String
  def aux (msg: String, key: Array[Char]) : String =
    msg.toArray.foldLeft("",0){(s, c) => (s._1 + (c+key(s._2 % key.length)).toChar, s._2+1)}._1
  
  def encrypt (msg: String, key: Key[Array[Char]]): String  =
    aux (msg, key.getPublic)

  def decrypt (msg: String, key : Key[Array[Char]]): String =
    aux (msg, key.getPrivate)
}

val toto = "testolivier"
val k = new VigenereKey (util.Random.nextInt())
val testCypher = new Vigenere
println(testCypher.decrypt(testCypher.encrypt(toto, k),k))



