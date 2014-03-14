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

class CesarKey (seed: int)  extends Key [Int] := 
{ 
  def generate(seed: int) =  
  {
     content = ((seed % 256) + 255) % 256
  }
  val content  = generate(seed)

  def getPublic () = content
  def getPrivate() = content
}

class CesarCypher extends GenericCypher
{
    def encrypt (msg: String, key: String): String :=
    {
       
    }
}
