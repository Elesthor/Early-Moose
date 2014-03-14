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

class CesarKey (seed: Int)  extends Key [Int]  
{
  def generate(seed: Int) =  
  {
    ((seed % 256) + 255) % 256
  }
  val content  = generate(seed)

  def getPublic () = content
  def getPrivate() = content
}

class CesarCypher extends GenericCypher [Int]
{
    def encrypt (msg: String, key: CesarKey): String =
    {
      var encoded = msg.getBytes
      for (i <- 0 to encoded.length)
      {
        encoded(i) = (encoded(i) + key.getPublic).toByte
      }
      return new String(encoded)
    }

    def decrypt (crypt: String, key: CesarKey): String =
        encrypt(crypt, key)
}

