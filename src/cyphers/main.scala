////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [Application.scala]                                              //
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

object Application
{
  def main(args: Array[String]): Unit =
  {
    // option ligne de commande
    if(args.length < 2 || (args(0) != "--RSA" && 
                           args(0) != "--Vigenere"
                           args(0) != "--Cesar"
                           args(0) != "--Enigma"
                           args(0) != "--Elgamal"
                           args(0) != "--AES"
                           ))
    {
      System.err.println("Please precise a cryptosystem among Cesar, Vigenere, RSA, Enigma, Elgamal, Enigma and AES and an input string")
    }
    else
    {
    }
  }
}
