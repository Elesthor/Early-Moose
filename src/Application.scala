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
    if(args.length < 2 || (args(0) != "-sync" && args(0) != "-async"))
    {
      System.err.println("Please precise -sync or -async and then a source file")
    }
    else
    {
      try
      {
        val src = new InputFromFile(args(1))
        val p = new Parser(src)
        try
        {
          // parsing
          println("--   Parsing    --")
          val program = p.parse()

          // playing
          (new Interpretor((args(0) == "-sync"))).interpret(program)
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
          System.err.println("file " + args(1) + " not found")
      }
    }
  }
}
