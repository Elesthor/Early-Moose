////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [input.scala]                                                    //
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
  
  // Types
  type Ch                   = Char
  type Checker              = Ch => Boolean

  // Current position in the file
  var line                  = 1 // Current line in the file
  var col                   = 0 // Current colonm in the file

  // Current char read, waiting for analysing
  var peeked: Option[Ch]    = None

  // Reading function of the concrete class
  def GetChar_(): Ch

  // Extended reading function : ignores comments and counts line and col
  def GetChar(): Ch =
  {
    GetChar_() match
    {
      case '%' =>
        col = col + 1
        IgnoreLine()
        GetChar()
      case '\n' =>
        line = line + 1
        col = 0
        '\n'
      case c    =>
        col = col + 1
        c
    }
  }

  // Ignore the end of the current line, for comments
  def IgnoreLine()
  {
    try
    {
      GetChar_() match
      {
        case '\n' =>
          line = line + 1
          col = 0
        case c    =>
          col = col + 1
          IgnoreLine()
      }
    }
    catch
    {
      case EndOfFile() => ()
    }
  }

  // Get the next char from peeked or input
  // Store it in peeked and return it
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

  // Consume next char and check if it is expected
  def GetCharPeekable(expected: Checker): Ch =
  {
    val c = Peek()
    CleanPeek()
    if(expected(c))
      c
    else
      throw new Unexpected(c, expected);
  }

  // Get a full word until a delimiter, using only char from expected
  // Let the delimiter in peeked. End on EOF
  def GetWord(expected: Checker, delimiters: Checker): String =
  {
    if(CheckEOF()) ""
    else
    {
      val c = Peek()
      if(expected(c))
      {
        CleanPeek()
        c+GetWord(expected, delimiters)
      }
      else if(delimiters(c))
        ""
      else
        throw new Unexpected(c, { x:Ch => expected(x) || delimiters(x) });
    }
  }

  // Consume a word from input
  // Throw Unexpected or End_of_file if it doesn't match
  def CheckNextWord(word: String) =
  {
    if (word != "")
    {
      val c = Peek()
      CleanPeek()
      if(c == word(0))
        CheckNextWord(word.reverse.dropRight(1).reverse) // TODO : abominable
      else
        throw Unexpected(c, IsChar(word(0)))
    }
  }

  // Get a number from input
  def GetNumber() =
  {
    Integer.parseInt(GetWord(Numeric, All))
  }

  // Try if EOF
  // else, let next char in peeked
  def CheckEOF() =
  {
    try
    {
      Peek()
      false
    }
    catch
    {
      case EndOfFile() => true
    }
  }

  // Ignore space characters, returns the number of ' ' ignored
  // Let first non-space in peeked
  def IgnoreSpace(): Int =
  {
    try
    {
      val c = Peek()
      if(c == '\n' || c == '\t')
      {
        CleanPeek()
        IgnoreSpace()
      }
      else if(c == ' ')
      {
        CleanPeek()
        IgnoreSpace() + 1
      }
      else
        0
    }
    catch
    {
      case EndOfFile() => 0
    }
  }

  // Set of checkers, which decide wether a char belongs to a certain subset of
  // the printable ascii alphabet.
  // TODO : faire une classe Checker, avec un attribut serialized genre "a-z",
  // le || sur les checker les 'compose', en concatenant les serialized
  def Numeric       (x: Ch) = { x >= '0' && x <= '9' }
  def AlphaLow      (x: Ch) = { x >= 'a' && x <= 'z' }
  def AlphaUp       (x: Ch) = { x >= 'A' && x <= 'Z' }
  def Alpha         (x: Ch) = { AlphaLow(x) || AlphaUp(x) }
  def AlphaNumeric  (x: Ch) = { Alpha(x) || Numeric(x) }
  def Parenthesis   (x: Ch) = { x == '(' || x == ')' }
  def Space         (x: Ch) = { x == ' ' || x == '\n' || x == '\t'}
  def All           (x: Ch) = { true }
  def IsChar        (c: Ch)(x: Ch) = { x == c } // Test if input is a given char
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
  def GetChar_() =
  {
    val c = inStream.read()
    if (c== -1) throw new EndOfFile()
    Character.toChars(c)(0)       // convert Int to Char
  }
}

