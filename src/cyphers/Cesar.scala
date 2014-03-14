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

class CesarKey (seed: Int ) extends Key [Char]
{
  def getPublic = seed.toChar
  def getPrivate = (-getPublic).toChar
}

class CesarCypher extends GenericCypher [Char]
{
  def encode(msg: String, key: Char): String =
    msg.toArray.foldLeft(""){(s,c) => s + (c + key).toChar}

  def encrypt (msg: String, key: Key [Char]): String =
    encode(msg, key.getPublic)

  def decrypt (crypt: String, key: Key [Char]): String =
    encode(crypt, key.getPrivate)
}

val toto = "testthomas"
val k = new CesarKey (util.Random.nextInt())
val testCypher = new CesarCypher
println(testCypher.decrypt(testCypher.encrypt(toto, k),k))

