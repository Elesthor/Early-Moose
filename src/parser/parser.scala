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
  var line                = 0                 // Current line in the file
  var col                 = 0                 // Current colonm in the file

  // Current char read, waiting for analysing
  var peeked: Option[Ch]  = None

  // Reading function of the concrete class
  def GetChar(): Ch

  type Ch                   = Char
  type Checker              = Char => Boolean

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

  // Get the next number present in the file
  def GetNumber() =
  {
    Integer.parseInt(GetWord(Numeric, {x => true}))
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

  def ParseConstant(delimiters: src.Checker) =
  {
    new VConst(src.GetWord(src.Alpha, delimiters))
  }

  def ParseProcess(): Process =
  {
    val keyword = src.GetWord(src.Alpha, {x: Char => src.Parenthesis(x) || x == ' ' || x == '^' || x == '0'})
    val peeked = src.GetCharPeekable(src.All)
    (keyword, peeked) match
    {
      case ("in", '(') =>
        val channel = ParseChannel(src.IsChar(','))
        val variable = ParseVariable(src.IsChar(')'))
        src.Peek()
        src.GetCharPeekable(src.IsChar('.'))
        new PIn(channel, variable, ParseProcess())

      case ("in", '^') =>
        new PTrivial()
        /*val k = src.GetNumber()
        src.GetCharPeekable(src.IsChar('('))

        val channel = ParseChannel()
        src.GetCharPeekable(src.IsChar(','))

        val variable = ParseVariable(src.IsChar('-'))
        // no need to check if peeked is Some('-')
        src.Peek(src.IsChar('>'))

        val u = ParseTerm()
        //...
        */




      case ("out", '(') =>
        val channel = ParseChannel(src.IsChar(','))
        val message = ParseTerm(src.IsChar(')'))
        src.Peek()
        src.GetCharPeekable(src.IsChar('.'))
        new PIn(channel, message, ParseProcess())

      case ("if", ' ') =>
        /*val value = ParseTerm(src.IsChar(' '))
        val then = src.GetWord(src.Alpha, src.IsChar(' '))
        if (then == "then") throw new SyntaxError(src.line, src.col)
        val P1 = ParseProcess()
        src.Peek()
        src.GetCharPeekable(src.IsChar('.'))
        new PIf(value, P1, ParseProcess())
      */


      case ("new", ' ') =>
        new PNew(ParseConstant(IsChar('.')), ParseProcess())
      case ("", '0') => new PTrivial
      case (_, _) => throw SyntaxError(src.line, src.col)
    }
  }

  /*def parseTerm(delimiters: src.Checker):Term
  {

  }

  def parseList():ListTerm
  {

  }*/
  
  def parseTerm():Value
  {
    val keyword = src.GetWord({ x: Char => src.Alpha(x) || src.Numeric(x)}  , {x: Char => src.Parenthesis(x) || x == ' ' || x == '[' || x == ':' || x == '>' || x == '=' || x == ','}) // TODO : rajouter v et ^
    val peeked = src.GetCharPeekable(src.All)
    (keyword, peeked) match
    {
      case ("pair", '(') =>
        val left = parseTerm()
        src.GetCharPeekable(src.IsChar(','))
        val right = parseTerm()
        src.GetCharPeekable(src.IsChar(')'))
        new TPair(left, right)
      case ("pi1", '(') =>
        val t = parserTerm()
        src.GetCharPeekable(src.IsChar(')'))
        new TPi1(t)
      case ("pi2", '(') =>
        val t = parserTerm()
        src.GetCharPeekable(src.IsChar(')'))
        new TPi2(t)
      case ("enc", '(') =>
        val left = parserTerm()
        src.GetCharPeekable(src.IsChar(','))
        val right = parseTerm()
        src.GetCharPeekable(src.IsChar(')'))
        new TEnc(left, right)
      case ("dec", '(') =>
        val left = parserTerm()
        src.GetCharPeekable(src.IsChar(','))
        val right = parseTerm()
        src.GetCharPeekable(src.IsChar(')'))
        new TDec(left, right)
      case ("pk", '(') =>
        val v = parserTerm() // TODO : ça doit etre une valeur
        src.GetCharPeekable(src.IsChar(')'))
        new TPk(v)
      case ("sk", '(') =>
        val v = parserTerm() // TODO : ça doit etre une valeur
        src.GetCharPeekable(src.IsChar(')'))
        new TSk(v)
      case ("", '[') =>
        src.GetCharPeekable(src.IsChar(']'))
        new ListTerm(None)
      case ("count", '(') =>
        val l = parseList()
        src.GetCharPeekable(src.IsChar(')'))
        new VCount(l)
      case ("not", '(') =>
        val v = parseTerm() // TODO : ça doit etre une valeur
        src.GetCharPeekable(src.IsChar(')'))
        new VCount(l)
      // TODO : comment on différencie les variables des constantes ?
    }
    
    // ici regarder si on a :: (on peut déjà avoir lu le premier :), =, > ... (peuvent etre déjà lus
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

