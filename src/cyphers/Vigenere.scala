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

class VigenereKey(seed: Int) extends Key[Array[Byte]]
{
  val publ = generate()
  val priv = publ.map({x => (-x).toByte})
  
  def generate() : Array[Byte] =
  {
    // random Array[Byte] of random size
    val randomizer = new util.Random(seed)
    val a = new Array[Byte](randomizer.nextInt() % 64 + 64)
    randomizer.nextBytes(a)
    a
  }
  
  def getPublic () = publ
  def getPrivate() = priv
}

class Vigenere extends GenericCypher[Array[Byte]]
{
  // we have to use a charset for convertion between Array[Byte] and String
  val charset = java.nio.charset.Charset.forName("ISO-8859-1")
  def aux (msg: String, key: Array[Byte]) : String =
  {
    var encoded = msg.getBytes(charset)
    for (i <- 0 to encoded.length-1)
      encoded(i) = (encoded(i) + key(i%key.length)).toByte
    new String(encoded, charset)
  }
  
  def encrypt (msg: String, key: Key[Array[Byte]]): String  =
  {
    aux (msg, key.getPublic)
  }
  def decrypt (msg: String, key : Key[Array[Byte]]): String =
  {
    aux (msg, key.getPrivate)
  }
}


var encoder = new Vigenere
var key = new VigenereKey(0)
var cypher = encoder.encrypt("coucou", key)
println(cypher)
println(encoder.decrypt(cypher, key))


