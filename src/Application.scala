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
  case class InvalidArgument(s: String) extends Exception
  
  // crée des cryptosystème en fonction de leur nom
  def cryptoMaker(cryptoName: String): EncapsulatedCrypto =
  {
    val regexRSA = "RSA([0-9]*)".r
    val regexEG  = "elGamal(|ec|zpmul|zpadd|)([0-9]+)".r
    cryptoName match
    {
      case "cesar"              =>
        new EncapsulatedCesar()
      case "vigenere"           =>
        new EncapsulatedVigenere()
      case regexRSA(keysize)    =>
        keysize match
        {
          case "" =>
            new EncapsulatedRsa(1024)
          case _  =>
            new EncapsulatedRsa(keysize.toInt)
        }
      case regexEG (mode, size) =>
        (mode, size) match
        {
          case ("", "") =>
            new EncapsulatedElGamalZp( // TODO générateur ?
              BigInt("20988936657440586486151264256610222593863921"), 7)
          case ("ec", "") =>
            new EncapsulatedElGamalEc()
          case ("zpmul", size) if size != "" =>
            val n = BigInt(size)
            if(!n.isProbablePrime(1000))
            {
              throw InvalidArgument("Not prime")
            }
            new EncapsulatedElGamalZp(n, 7) // TODO générateur ?
          case ("zpadd", size) if size != "" =>
            new EncapsulatedElGamalZk(BigInt(size))
          case _ =>
            throw InvalidArgument("Bad crypto name")
        }
      case _ =>
        throw InvalidArgument("Bad crypto name")
    }
  }
  
  def main(args: Array[String]): Unit =
  {
    val moose ="////////////////////////////////////////////////////////////////////////////////\n//                                                                            //\n//                                                   \\_\\_    _/_/             //\n//      .--       .      .  .                            \\__/                 //\n//      |- .-. .-.| . .  |\\/|.-..-..-.-,                 (oo)\\_______   /     //\n//      '--`-`-'  '-'-|  '  '`-'`-'-'`'-                 (__)\\       )\\/      //\n//                  `-'                                      ||-----||        //\n//                                                           ||     ||        //\n////////////////////////////////////////////////////////////////////////////////"
    println(moose)
    // option ligne de commande
    if(args.length < 3 || (args(0) != "-sync" && args(0) != "-async"))
    {
      System.err.println("Please precise -sync or -async, then a source file and then a cryptosystem among...") // TODO
    }
    else
    {
      // lecture des arguments
      val mode = args(0) == "-sync"
      val filename = args(1)
      val crypto =
        try
        {
          args(2) match
          {
            case "-cesar"    => new EncapsulatedCesar()
            case "-vigenere" => new EncapsulatedVigenere()
            case "-RSA"      =>
              if(args.length > 3)
              {
                if(args.length > 4 && args(3) == "-keysize")
                  new EncapsulatedRsa(args(4).toInt)
                else
                {
                  throw InvalidArgument("Bad option")
                }
              }
              else
                new EncapsulatedRsa(1024)
            case "-elGamal"  =>
              if(args.length > 3)
                (args.length, args(3)) match
                {
                  case (4, "-ec")    =>
                    new EncapsulatedElGamalEc()
                  case (5, "-zpadd") =>
                    new EncapsulatedElGamalZk(BigInt(args(4)))
                  case (5, "-zpmul") =>
                    val n = BigInt(args(4))
                    if(!n.isProbablePrime(1000))
                    {
                      throw InvalidArgument("Not prime")
                    }
                    new EncapsulatedElGamalZp(n, 7) // TODO générateur ?
                  case _ =>
                    throw InvalidArgument("Bad option")
                }
              else
                new EncapsulatedElGamalZp(
                  BigInt("20988936657440586486151264256610222593863921"), 7) // TODO générateur ?
            case _ =>
              throw InvalidArgument("Bad option")
          }
        }
        catch
        {
          case _ :java.lang.NumberFormatException =>
            throw InvalidArgument("Bad option : not an integer")
        }
      
      try
      {
        val src = new InputFromFile(filename)
        val p = new Parser(src)
        try
        {
          // parsing
          println("--   Parsing    --")
          val program = p.parse()

          // playing
          (new Interpretor(mode, crypto, cryptoMaker)).interpret(program)
        }
        catch
        {
          case p.SyntaxError()       =>
            System.err.println("Syntax Error (line " + src.line + "; col " + src.col + ")\n")
            System.exit(1)
          case p.NameMalformed(name) =>
            System.err.println("Malformed name : '" + name + "' (line " + src.line + "; col " + src.col + ")\n")
            System.exit(1)
          case src.EndOfFile()       =>
            System.err.println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
            System.exit(1)
          case src.Unexpected(c, f)  =>
            System.err.println("Character '" + src.charToString(c) + "' unexpected (line " + src.line + "; col " + src.col + ")\nExpected : " + f.serialized)
            System.exit(1)
        }
      }
      catch
      {
        case _:java.io.FileNotFoundException =>
          System.err.println("file " + filename + " not found")
          System.exit(1)
      }
    }
  }
}
