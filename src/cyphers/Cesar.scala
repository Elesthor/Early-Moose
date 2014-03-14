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

class Cesar extends GenericCypher [Char]
{
  def encode(msg: String, key: Char): String =
    msg.toArray.foldLeft(""){(s,c) => s + (c + key).toChar}

  def encrypt (msg: String, key: Key [Char]): String =
    encode(msg, key.getPublic)

  def decrypt (crypt: String, key: Key [Char]): String =
    encode(crypt, key.getPrivate)
}

