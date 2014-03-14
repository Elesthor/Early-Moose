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
  val content  = generate(seed)

  def getPublic () = content
  def getPrivate() = content
}

class CesarCypher extends GenericCypher [Byte]
{
  def encode(msg: String, key: Byte): String =
  {
    new String(msg.getBytes.map({x => (x + key).toByte})) // TODO buggé
  }
  def encrypt (msg: String, key: Key [Byte]): String =
  {
    encode(msg, key.getPublic)
  }

  def decrypt (crypt: String, key: Key [Byte]): String =
  {
    encode(crypt, (-key.getPrivate).toByte)
  }
}

val toto = "testthomas"
val k = new CesarKey (util.Random.nextInt())
val testCypher = new CesarCypher
println(testCypher.decrypt(testCypher.encrypt(toto, k),k))

