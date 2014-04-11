////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [TestCyphers.scala]                                              //
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

import java.io.FileInputStream
import java.io.File

object TestCyphers
{
  val ERROR = "Please precise --cryptosystem (among Cesar, Vigenere, RSA, Enigma, ElGamal, Enigma and AES) --mode (among encrypt and decrypt) seed (for generating a random key) [input] (string, or if omitted : read from stdin)\nExample : scala TestCyphers --ElGamal --encrypt 10 coucou | scala TestCyphers --ElGamal --decrypt 10"
  def main(args: Array[String]): Unit =
  {
    // option ligne de commande
    if(args.length < 3 || (args(1) != "--encrypt" && args(1) != "--decrypt"))
      System.err.println(ERROR)
    else
    {
      val seed = args(2).toInt
      val msg = if(args.length == 3) // read from stdin
      {
        var buffer = ""
        var tmp = new Array[Char](1024)
        var readen = 0
        do
        {
          readen = Console.in.read(tmp, 0, 1024)
          buffer = buffer ++ tmp.slice(0, readen)
        } while(readen > 0);
        buffer.dropRight(1)
      }
      else
        args(3)
      val encrypt = args(1) == "--encrypt"
      args(0) match
      {
        case "--RSA"      => 
        { 
          val cypher = new Rsa()
          val key = new RsaKey(seed)
          if(encrypt)
            print(cypher.encrypt(msg, key, seed))
          else print(cypher.decrypt(msg, key))
        }

        case "--Vigenere" =>
        { 
          val cypher = new Vigenere()
          val key = new VigenereKey(seed)
          if(encrypt)
            print(cypher.encrypt(msg, key, seed))
          else print(cypher.decrypt(msg, key))
        }
      
        case "--Cesar"    =>
        {
          val cypher = new Cesar()
          val key = new CesarKey(seed)
          if(encrypt)
            print(cypher.encrypt(msg, key, seed))
          else print(cypher.decrypt(msg, key))
        }
        case "--Enigma"   =>
        {
          val cypher = new Enigma()
          val key = new EnigmaKey(seed)
          if(encrypt)
            print(cypher.encrypt(msg, key, seed))
          else print(cypher.decrypt(msg, key))
        }

        case"--ElGamal"   =>
        {
          //val grp = new Zp(BigInt("131071"), 3)
          val f = new Zpf(2147483647)
          val grp = new Elliptic[BigInt](f, 1, (1, 3), 1547483647)
          val cypher = new ElGamal(grp)
          val key = new ElGamalKey(grp, seed)
          if(encrypt)
            println(cypher.encrypt(msg, key, seed))
          else print(cypher.decrypt(msg, key))
        }
        
        case "--AES"      =>
        {
          val cypher = new AES()
          val key = new AESKey(seed)
          if(encrypt)
            println(cypher.encrypt(msg, key))
          else println(cypher.decrypt(msg, key))
        }
        case _ => System.err.println(ERROR)
      }
    }
  }
}
