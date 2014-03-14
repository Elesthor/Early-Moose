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
  val publ = generate()
  val priv = publ.map({x => (-x).toChar})
  
  def generate() : Array[Char] =
  {
    // random Array[Char] of random size
    val randomizer = new util.Random(seed)
    randomizer.nextString(randomizer.nextInt() % 64 + 64).toArray
  }
  
  def getPublic () = publ
  def getPrivate() = priv
}

class Vigenere extends GenericCypher[Array[Char]]
{
  // we have to use a charset for convertion between Array[Char] and String
  def aux (msg: String, key: Array[Char]) : String =
  {
    msg.toArray.foldLeft("",0){(s, c) => (s._1 + (c+key(s._2 % key.length)).toChar, s._2+1)}._1
  }
  
  def encrypt (msg: String, key: Key[Array[Char]]): String  =
  {
    aux (msg, key.getPublic)
  }
  def decrypt (msg: String, key : Key[Array[Char]]): String =
  {
    aux (msg, key.getPrivate)
  }
}


var encoder = new Vigenere
var key = new VigenereKey(util.Random.nextInt)
var cypher = encoder.encrypt("coucou", key)
println(cypher)
println(encoder.decrypt(cypher, key))


