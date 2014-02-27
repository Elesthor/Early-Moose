////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [main.scala]                                                     //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//


object Application
{
  def main(args: Array[String]): Unit =
  {
    // option ligne de commande
    if(args.length < 2 || (args(0) != "-sync" && args(0) != "-async"))
    {
      System.err.println("please precise -sync or -async and then a source file")
    }
    else
    {
      val t = new Interpretor(args(0)=="-sync")
      
      // parsing
      val src = new InputFromFile(args(1))
      val p = new Parser(src)
      try
      {
        val program = p.Parse()
        println("end of parsing")
        
        // playing
        (new Interpretor(args(0) == "-sync")).InterpretMetaProc(program)
      }
      catch
      {
        case p.SyntaxError()       => println("Syntax Error (line " + src.line + "; col " + src.col + ")\n")
        case p.ValueExpected()     => println("A value was expected (line " + src.line + "; col " + src.col + ")\n")
        case p.NameMalformed(name) => println("Malformed name : '" + name + "' (line " + src.line + "; col " + src.col + ")\n")
        case src.EndOfFile()       => println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
        case src.Unexpected(c, f)  =>
          println("Character '" + src.CharToString(c) + "' unexpected (line " + src.line + "; col " + src.col + ")\nExpected : " + f.serialized)
      }
    }
  }
}
