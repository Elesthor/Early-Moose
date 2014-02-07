////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [parser.scala]                                                   //
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
//                              Input class                                   //
////////////////////////////////////////////////////////////////////////////////
//
// Utility class to handle a stream

abstract class Input
{

  // Errors and limit cases
  case class Unexpected(c:Ch, expected: Checker)  extends Exception
  case class EndOfFile()                          extends Exception

  // Current position in the file
  var line                  = 1                 // Current line in the file
  var col                   = 0                 // Current colonm in the file

  // Current char read, waiting for analysing
  var peeked: Option[Ch]    = None

  // Reading function of the concrete class
  def GetChar(): Ch

  type Ch                   = Char
  type Checker              = Ch => Boolean

  // Set of checkers, which decide wether a char belongs to a certain subset of
  // the printable ascii alphabet.
  def Numeric       (x: Ch) = { x >= '0' && x <= '9' }
  def AlphaLow      (x: Ch) = { x >= 'a' && x <= 'z' }
  def AlphaUp       (x: Ch) = { x >= 'A' && x <= 'Z' }
  def Alpha         (x: Ch) = { AlphaLow(x) || AlphaUp(x) }
  def AlphaNumeric  (x: Ch) = { Alpha(x) || Numeric(x) }
  def Parenthesis   (x: Ch) = { x == '(' || x == ')' }
  def All           (x: Ch) = { true }
  def IsChar        (c: Ch)(x: Ch) = { x == c } // Test if input is a given char

  // If peeked isn't empty, read from it, else read with GetChar
  def GetCharPeekable(expected: Checker): Ch =
  {
    val c = Peek()
    CleanPeek()
    if(expected(c))
      c
    else
      throw new Unexpected(c, expected);
  }

  // Get the next char
  def Peek(): Ch =
  {
    peeked match
    {
      case Some(c) => c
      case None    =>
        val c = GetChar()
        peeked = Some(c)
        c
    }
  }

  // Clean the peeked char
  def CleanPeek() =
  {
    peeked = None
  }

  // Get a full word until a delimiter, using only char from expected. Let the delimiter in peeked. End on EOF
  def GetWord(expected: Checker, delimiters: Checker): String =
  {
    def iterator(): String =
    {
      if(CheckEOF()) ""
      else
      {
        val c = Peek()
        if(expected(c))
        {
          CleanPeek()
          c+iterator()
        }
        else if(delimiters(c))
          ""
        else
          throw new Unexpected(c, expected);
      }
    }
    iterator()
  }


  def CheckNextWord(word: String): Boolean = {
    if (word == "") true
    else
    {
      if (GetCharPeekable(All) == word(0)) CheckNextWord(word.reverse.dropRight(1).reverse) // TODO : abominable
      else false
    }
  }

  // Get the next number present in the file
  def GetNumber() =
  {
    Integer.parseInt(GetWord(Numeric, All))
  }

  // Try if EOF (else, char is in peeked)
  def CheckEOF() = 
  {
    try
    {
      Peek()
      false
    }
    catch
    {
      case e: EndOfFile => true
    }
  }
}


////////////////////////////////////////////////////////////////////////////////
//                           InputFrom File class                             //
////////////////////////////////////////////////////////////////////////////////
//
// Utility class to handle an input file


import java.io.FileInputStream
class InputFromFile(file:String) extends Input
{
  val inStream = new FileInputStream(file)

  // Main reading function
  def GetChar() =
  {
    val c = inStream.read()
    if(c == '\n')
    {
      line = line + 1;
      col = 0
    }
    else col = col + 1
    if (c== -1) throw new EndOfFile()
    Character.toChars(c)(0)       // convert Int to Char
  }
}



////////////////////////////////////////////////////////////////////////////////
//                                Parser class                                //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the parser


class Parser(src: Input)
{
  case class SyntaxError() extends Exception

  def ParseMetaProc() : MetaProc =
  {
    val left = ParseProcess()
    if(src.CheckEOF())
      new MetaProc(left, 1, None)
    else
    {
      val c = src.GetCharPeekable({ x:Char => x == '|' || x == '^'})
      if(c == '|')
      {
        src.CheckNextWord("|")
        new MetaProc(left, 1, Some(ParseMetaProc()))
      }
      else // c == '^'
      {
        val k = src.GetNumber()
        if(src.CheckEOF())
          new MetaProc(left, k, None)
        else
        {
          src.CheckNextWord("||")
          new MetaProc(left, k, Some(ParseMetaProc()))
        }
      }
    }
  }  
  
  def ParseVariable(delimiters: src.Checker) =
  {
    new TVar(src.GetWord(src.Alpha, delimiters))
  }

  def ParseChannel(delimiters: src.Checker) =
  {
    new Channel(src.GetWord(src.Alpha, delimiters))
  }

  def ParseConstant() =
  {
    new VConst(src.GetWord(src.Alpha, src.All))
  }

  def ParseProcessSeq() : Process =
  {
    if(src.CheckEOF())
      new PTrivial()
    else if(src.Peek() == '.') // séquence
    {
      src.CleanPeek()
      ParseProcess()
    }
    else
      new PTrivial()
  }

  def ParseProcess(): Process =
  {
    val keyword = src.GetWord(src.Alpha, {x: Char => src.Parenthesis(x) || x == ' ' || x == '^' || x == '0'})
    val peeked = src.GetCharPeekable(src.All)
    (keyword, peeked) match
    {
      case ("in", '(') =>
        val channel = ParseChannel(src.IsChar(','))
        src.CleanPeek()
        
        val variable = ParseVariable(src.IsChar(')'))
        src.CleanPeek()
        new PIn(channel, variable, ParseProcessSeq())

      case ("in", '^') =>
        new PTrivial()
        val k = src.GetNumber()
        src.CheckNextWord("(")

        val c = ParseChannel(src.IsChar(','))
        src.CleanPeek()

        val x = ParseVariable(src.IsChar('-'))
        src.CleanPeek()
        src.CheckNextWord(">")

        val u = ParseTerm()
        src.CheckNextWord(" as ")
        
        val y = ParseVariable(src.IsChar(')'))
        src.CleanPeek()

        new PInk(c, x, u, y, k, ParseProcessSeq())

      case ("out", '(') =>
        val channel = ParseChannel(src.IsChar(','))
        src.CleanPeek()
        
        val message = ParseTerm()
        src.CheckNextWord(")")
        new POut(channel, message, ParseProcessSeq())

      case ("if", ' ') =>
        val value = ParseTerm()
        src.CheckNextWord(" then ")
        val P1 = ParseProcess()
        src.CheckNextWord(" else ")
        new PIf(value, P1, ParseProcessSeq())

      case ("new", ' ') =>
        new PNew(ParseConstant(), ParseProcessSeq())
      case ("", '0') => new PTrivial
      case (_, _) => throw new SyntaxError()
    }
  }

  def ParseList():List[Term] =
  {
    List()
  }

  def ParseTerm():Term =
  {
    val c = src.Peek()
    val leftTerm:Term =
      if(src.Numeric(c))
        new TValue(new VInt(src.GetNumber()))
      else
      {
        val keyword = src.GetWord({ x: Char => src.Alpha(x) || src.Numeric(x)},
                                  { x: Char => src.Parenthesis(x) || x == ' '|| x == '[' || x == ':' || x == '>' || x == '=' || x == '/' || x == '\\'})
        val peeked = src.GetCharPeekable(src.All)
        (keyword, peeked) match
        {
          case ("pair", '(') => 
            val left = ParseTerm()
            src.CheckNextWord(",")
            val right = ParseTerm()
            src.CheckNextWord(")")
            new TPair(left, right)
          case ("pi1", '(') =>
            val t = ParseTerm()
            src.CheckNextWord(")")
            new TPi1(t)
          case ("pi2", '(') =>
            val t = ParseTerm()
            src.CheckNextWord(")")
            new TPi2(t)
          case ("enc", '(') =>
            val left = ParseTerm()
            src.CheckNextWord(",")
            val right = ParseTerm()
            src.CheckNextWord(")")
            new TEnc(left, right)
          case ("dec", '(') =>
            val left = ParseTerm()
            src.CheckNextWord(",")
            val right = ParseTerm()
            src.CheckNextWord(")")
            new TDec(left, right)
          case ("pk", '(') =>
            val v = ParseTerm()
            src.CheckNextWord(")")
            new TPk(v)
          case ("sk", '(') =>
            val v = ParseTerm()
            src.CheckNextWord(")")
            new TSk(v)
          
          // Lists
          case ("", '[') =>
            src.CheckNextWord("]")
            new ListTerm(List())
          
          // Values
          case ("count", '(') =>
            val l = new ListTerm(ParseList())
            src.CheckNextWord(")")
            new TValue(new VCount(l))
          case ("not", '(') =>
            val v = ParseTerm()
            src.CheckNextWord(")")
            new TValue(new VNot(v))
          
          case (head, ':') => // début de liste avec une variable
            src.CheckNextWord(":")
            new ListTerm(new TVar(head) :: ParseList())
          case (left, '/') => // and avec une constante
            src.CheckNextWord("\\")
            new TValue(new VAnd(new TValue(new VConst(left)), ParseTerm()))
          case (left, '\\') => // or avec une constante
            src.CheckNextWord("/")
            new TValue(new VOr(new TValue(new VConst(left)), ParseTerm()))
          case (left, '=') => // = avec une variable
            new TValue(new VEqual(new TVar(left), ParseTerm()))
          case (left, '>') => // > avec une constante
            new TValue(new VSup(new TValue(new VConst(left)), ParseTerm()))
        }
      }
    
    // si le caractère suivant est un opérateur binaire
    val next = src.Peek()
    next match
    {
      case ':'  =>
        src.CleanPeek()
        src.CheckNextWord(":")
        new ListTerm(leftTerm :: ParseList())
      case '/'  =>
        src.CleanPeek()
        src.CheckNextWord("\\")
        new TValue(new VAnd(leftTerm, ParseTerm()))
      case '\\' =>
        src.CleanPeek()
        src.CheckNextWord("/")
        new TValue(new VOr(leftTerm, ParseTerm()))
      case '='  =>
        src.CleanPeek()
        new TValue(new VEqual(leftTerm, ParseTerm()))
      case '>'  =>
        src.CleanPeek()
        new TValue(new VSup(leftTerm, ParseTerm()))
      case _    => leftTerm
    }
  }
}

object TestParser
{
  def main(args: Array[String])
  {
    val src = new InputFromFile("test")
    val p = new Parser(src)
    try
    {
      println(p.ParseMetaProc().RetString(0))
      println("end of parsing")
    }
    catch
    {
      case p.SyntaxError()     => println("Syntax Error (line " + src.line + "; col " + src.col + ")\n")
      case src.EndOfFile()       => println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
      case src.Unexpected(c, f)  => println("Character '" + c + "' unexpected (line " + src.line + "; col " + src.col + ")\n")
    }
  }
}

