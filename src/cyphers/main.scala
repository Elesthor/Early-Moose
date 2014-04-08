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

val ERROR = "Please precise a cryptosystem among Cesar, Vigenere, RSA, Enigma, Elgamal, Enigma and AES, a seed for generating a random key and an input string"
 
object Application
{
  def main(args: Array[String]): Unit =
  {
    // option ligne de commande
    if(args.length < 3 || (args(0) != "--RSA" && 
                           args(0) != "--Vigenere"
                           args(0) != "--Cesar"
                           args(0) != "--Enigma"
                           args(0) != "--Elgamal"
                           args(0) != "--AES"
                           ))
    
      System.err.println(ERROR)
    else
    {
      args(0) match
      {
        case "--RSA"      => 
        { 
          val cypher = new RSA()
          val key = new RsaKey(args(2))
          if (args(1) = "encrypt")
          {
            cypher.encrypt(args(3), key, args(2))
          }
          else cypher.encrypt(args(3), key, args(2))
        }

        case "--Vigenere" =>
        { 
          val cypher = new Vigenere()
          val key = new VigenereKey(args(2))
          if (args(1) = "encrypt")
          {
            cypher.encrypt(args(3), key, args(2))
          }
          else cypher.encrypt(args(3), key, args(2))
        }
      
        case "--Cesar"    =>
        {
          val cypher = new Cesar()
          val key = new CesarKey(args(2))
          if (args(1) = "encrypt")
          {
            cypher.encrypt(args(3), key, args(2))
          }
          else cypher.encrypt(args(3), key, args(2))
        }
        case "--Enigma"   =>
        {
          val cypher = new Cesar()
          val key = new EnigmaKey(args(2))
          if (args(1) = "encrypt")
          {
            cypher.encrypt(args(3), key, args(2))
          }
          else cypher.encrypt(args(3), key, args(2))
        }

        case"--Elgamal"   =>
          {
          val cypher = new ElGamal()
          val key = new ElGamalKey(args(2))
          if (args(1) = "encrypt")
          {
            cypher.encrypt(args(3), key, args(2))
          }
          else cypher.encrypt(args(3), key, args(2))
        }
        case "--AES"      =>
        {
          val cypher = new AES()
          val key = new AESKey(args(2))
          if (args(1) = "encrypt")
          {
            cypher.encrypt(args(3), key, args(2))
          }
          else cypher.encrypt(args(3), key, args(2))
        }
      }
    }
  }
}
