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
          { // TODO : seed bigint
            case "-vigenere" => new EncapsulatedVigenere()
            case "-cesar"    => new EncapsulatedCesar()
            case "-RSA"      =>
              if(args.length > 3)
              {
                if(args.length > 4 && args(3) == "-keysize")
                  new EncapsulatedRsa(args(4).toInt)
                else
                {
                  System.err.println("Bad option")
                  System.exit(1)
                  new EncapsulatedCesar() // to make compiler happy
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
                    if(!n.isProbablePrime(10))
                    {
                      System.err.println("Not prime")
                      System.exit(1)
                    }
                    new EncapsulatedElGamalZp(n, 7) // TODO générateur ?
                  case _ =>
                    System.err.println("Bad option")
                    System.exit(1)
                    new EncapsulatedCesar() // to make compiler happy
                }
              else
                new EncapsulatedElGamalZp(
                  BigInt("20988936657440586486151264256610222593863921"), 7) // TODO générateur ?
            case _ =>
              System.err.println("Bad option")
              System.exit(1)
              new EncapsulatedCesar() // to make compiler happy
          }
        }
        catch
        {
          case _ :java.lang.NumberFormatException =>
            System.err.println("Bad option : not an integer")
            System.exit(1)
            new EncapsulatedCesar() // to make compiler happy
        }
      
      println(crypto.encrypt("coucou", crypto.makeKey(1)))
      
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
          (new Interpretor(mode)).interpret(program)
        }
        catch
        {
          case p.SyntaxError()       =>
            System.err.println("Syntax Error (line " + src.line + "; col " + src.col + ")\n")
          case p.NameMalformed(name) =>
            System.err.println("Malformed name : '" + name + "' (line " + src.line + "; col " + src.col + ")\n")
          case src.EndOfFile()       =>
            System.err.println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
          case src.Unexpected(c, f)  =>
            System.err.println("Character '" + src.charToString(c) + "' unexpected (line " + src.line + "; col " + src.col + ")\nExpected : " + f.serialized)
        }
      }
      catch
      {
        case _:java.io.FileNotFoundException =>
          System.err.println("file " + filename + " not found")
      }
    }
  }
}
