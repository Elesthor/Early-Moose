////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Cesar.scala]                                                  //
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

class CesarKey (seed: Int ) extends Key [Byte]
{
  def generate(seed: Int) =
  {
    seed.toByte
  }
  val publ = generate(seed)
  val priv = (-publ).toByte

  def getPublic () = publ
  def getPrivate() = priv
}

class CesarCypher extends GenericCypher [Byte]
{
  val charset = java.nio.charset.Charset.forName("ISO-8859-1")
  def encode(msg: String, key: Byte): String =
  {
    new String(msg.getBytes(charset).map({x => (x + key).toByte}), charset)
  }
  def encrypt (msg: String, key: Key [Byte]): String =
  {
    encode(msg, key.getPublic)
  }

  def decrypt (crypt: String, key: Key [Byte]): String =
  {
    encode(crypt, key.getPrivate)
  }
}

val toto = "testthomas"
val k = new CesarKey (util.Random.nextInt())
val testCypher = new CesarCypher
println(testCypher.decrypt(testCypher.encrypt(toto, k),k))

