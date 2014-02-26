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

// Class of T=>Boolean functions, composable with ||
// that concatenate serialized
class GenericChecker[T](_f: T => Boolean, _serialized: String)
{
  val f = _f
  val serialized = _serialized

  def ||(right : GenericChecker[T]) =
  {
    new GenericChecker({x: T => f(x) || right.f(x)}, serialized+right.serialized)
  }
  def apply(x: T) = { f(x) }
}

abstract class Input
{
  // Errors and limit cases
  case class Unexpected(c:Ch, expected: Checker)  extends Exception
  case class EndOfFile()                          extends Exception

  // Type of char
  type Ch                   = Char
  type Checker              = GenericChecker[Ch]

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

  // Ignore space at beginning if ignoreSpace
  // then get a full word until a delimiter, using only char from expected
  // Let the delimiter in peeked. End on EOF
  def GetWord(expected: Checker, delimiters: Checker, ignoreSpace:Boolean = true): String =
  {
    if(ignoreSpace) IgnoreSpace()
    
    if(CheckEOF()) ""
    else
    {
      val c = Peek()
      if(expected(c))
      {
        CleanPeek()
        c+GetWord(expected, delimiters, false)
      }
      else if(delimiters(c))
        ""
      else
        throw new Unexpected(c, expected || delimiters);
    }
  }

  // Ignore space at beginning if ignoreSpace
  // then consume a word from input
  // Throw Unexpected or End_of_file if it doesn't match
  def CheckNextWord(word: String, ignoreSpace:Boolean = true):Unit =
  {
    if(ignoreSpace) IgnoreSpace()
    
    if (word != "")
    {
      val c = Peek()
      CleanPeek()
      if(c == word(0))
        CheckNextWord(word.reverse.dropRight(1).reverse, false) // TODO : abominable
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

  // Ignore space characters, returns the number of chars ignored
  // Let first non-space in peeked
  def IgnoreSpace(): Int =
  {
    try
    {
      val c = Peek()
      if(c == '\n' || c == '\t' || c == ' ')
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

  // Link a char to string representation
  def CharToString(x : Ch) =
  {
    x match
    {
      case '\n' => "\\n"
      case '\t' => "\\t"
      case ' '  => "␣"
      case a    => a.toString()
    }
  }

  // Set of checkers, which decide wether a char belongs to a certain subset of
  // the printable ascii alphabet.
  val Numeric               = new Checker({x => x >= '0' && x <= '9'}, "0-9")
  def AlphaLow              = new Checker({x => x >= 'a' && x <= 'z'}, "a-z")
  def AlphaUp               = new Checker({x => x >= 'A' && x <= 'Z'}, "A-Z")
  def Alpha                 = AlphaLow    || AlphaUp
  def AlphaNumeric          = Alpha       || Numeric
  def Parenthesis           = IsChar('(') || IsChar(')')
  def Space                 = IsChar(' ') || IsChar('\n') || IsChar('\t')
  def All                   = new Checker({x => true}, " all ")
  def IsChar        (c: Ch) = new Checker({x => x == c}, CharToString(c))
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
