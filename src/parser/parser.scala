////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [parser.scala]                                                   //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//


////////////////////////////////////////////////////////////////////////////////
//                              Input class                                   //
////////////////////////////////////////////////////////////////////////////////
//
// Utility class to handle a stream

abstract class Input
{

  // Errors and limit cases
  case class Unexpected(c:Ch, expected: Checker) extends Exception
  case class EndOfFile() extends Exception

  // Current position in the file
  var line                  = 0                 // Current line in the file
  var col                   = 0                 // Current colonm in the file

  // Current char read, waiting for analysing
  var peeked: Option[Ch]    = None

  // Reading function of the concrete class
  def GetChar(): Ch

  type Ch                 = Char
  type Checker            = Ch => Boolean

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
    val c =
      if(peeked.isEmpty) GetChar()
      else
      {
        val tmp = peeked.get
        peeked = None
        tmp
      }
    if(expected(c)) c
    else throw new Unexpected(c, expected);
  }

  // Get the next char
  def Peek(): Ch =
  {
    val c = GetChar()
    peeked = Some(c)
    c
  }

  // Clean the peeked char
  def CleanPeek() =
  {
    peeked = None
  }

  // Get a full word between the delimiters, using only char from expected
  def GetWord(expected: Checker, delimiters: Checker): String =
  {
    def iterator(): String ={
      val c = GetCharPeekable({ x: Ch => expected(x) || delimiters(x) })
      if (expected(c)) c+iterator()
      else ""
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
    if (c== -1) throw EndOfFile()
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
  case class SyntaxError(line:Int, col:Int) extends Exception

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
    if(src.Peek() == '.') // séquence
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
        new PNew(ParseConstant(), ParseProcess())
      case ("", '0') => new PTrivial
      case (_, _) => throw SyntaxError(src.line, src.col)
    }
  }

  def ParseList():List[Term] =
  {
    List()
  }

  def ParseTerm():Term =
  {
    val c = src.Peek()
    val left_term =
      if(src.Numeric(c))
        new VInt(src.GetNumber())
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
            new VCount(l)
          case ("not", '(') =>
            val v = ParseTerm()
            src.CheckNextWord(")")
            new VNot(v)
          
          case (head, ':') => // début de liste avec une variable
            src.CheckNextWord(":")
            new ListTerm(new TVar(head) :: ParseList())
          case (left, '/') => // and avec une variable
            src.CheckNextWord("\\")
            new VAnd(new TVar(left), ParseTerm())
          case (left, '\\') => // or avec une variable
            src.CheckNextWord("/")
            new VOr(new TVar(left), ParseTerm())
          case (left, '=') => // = avec une variable
            new VEqual(new TVar(left), ParseTerm())
          case (left, '>') => // > avec une variable
            new VSup(new TVar(left), ParseTerm())
        }
      }
    
    // si le caractère suivant est un opérateur binaire
    val next = src.Peek()
    next match
    {
      case ':'  =>
        src.CleanPeek()
        src.CheckNextWord(":")
        new ListTerm(left_term :: ParseList())
      case '/'  =>
        src.CleanPeek()
        src.CheckNextWord("\\")
        new VAnd(left_term, ParseTerm())
      case '\\' =>
        src.CleanPeek()
        src.CheckNextWord("/")
        new VOr(left_term, ParseTerm())
      case '='  =>
        src.CleanPeek()
        new VEqual(left_term, ParseTerm())
      case '>'  =>
        src.CleanPeek()
        new VSup(left_term, ParseTerm())
      case _    => left_term
    }
  }
}

object TestParser
{
  def main(args: Array[String])
  {
    val p = new Parser(new InputFromFile("test"))
    //p.ParseProcess().retString()
  }
}

