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

import perso.utils.NetworkTools._

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
  def getChar_(): Ch

  // Extended reading function : ignores comments and counts line and col
  def getChar(): Ch =
  {
    getChar_() match
    {
      case '%' =>
        col = col + 1
        ignoreLine()
        getChar()
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
  def ignoreLine()
  {
    try
    {
      getChar_() match
      {
        case '\n' =>
          line = line + 1
          col = 0
        case c    =>
          col = col + 1
          ignoreLine()
      }
    }
    catch
    {
      case EndOfFile() => ()
    }
  }

  // Get the next char from peeked or input
  // Store it in peeked and return it
  def peek(): Ch =
  {
    peeked match
    {
      case Some(c) => c
      case None    =>
        val c = getChar()
        peeked = Some(c)
        c
    }
  }

  // Clean the peeked char
  def cleanPeek() =
  {
    peeked = None
  }

  // Consume next char and check if it is expected
  def getCharPeekable(expected: Checker): Ch =
  {
    val c = peek()
    cleanPeek()
    if(expected(c))
      c
    else
      throw new Unexpected(c, expected);
  }

  // Ignore space at beginning if ignSpace
  // then get a full word until a delimiter, using only char from expected
  // Let the delimiter in peeked. End on EOF
  def getWord(expected: Checker, delimiters: Checker, ignSpace:Boolean = true): String =
  {
    if(ignSpace) ignoreSpace()
    
    if(checkEOF()) ""
    else
    {
      val c = peek()
      if(expected(c))
      {
        cleanPeek()
        c+getWord(expected, delimiters, false)
      }
      else if(delimiters(c))
        ""
      else
        throw new Unexpected(c, expected || delimiters);
    }
  }

  // Ignore space at beginning if ignSpace
  // then consume a word from input
  // Throw Unexpected or End_of_file if it doesn't match
  def checkNextWord(word: String, ignSpace:Boolean = true):Unit =
  {
    if(ignSpace) ignoreSpace()
    
    if (word != "")
    {
      val c = peek()
      cleanPeek()
      if(c == word(0))
        checkNextWord(word.drop(1), false)
      else
        throw Unexpected(c, isChar(word(0)))
    }
  }

  // Get a number from input
  def getNumber():BigInt =
  {
    BigInt(getWord(numeric || isChar('-'), all))
  }
  
  // Return len characters
  def getRaw(): String =
  {
    val len = getNumber().toInt
    checkNextWord("#")
    def aux(len: Int): String =
    {
      if(len == 0)
        ""
      else
        getChar_() + aux(len-1)
    }
    
    if(len < 0)
      assert(false)
    if(len == 0)
      ""
    else
      // first character may be in peeked
      peeked match
      {
        case Some(c) => peeked=None; c+aux(len-1)
        case None    => aux(len)
      }
  }

  // Try if EOF
  // else, let next char in peeked
  def checkEOF() =
  {
    try
    {
      peek()
      false
    }
    catch
    {
      case EndOfFile() => true
    }
  }

  // Ignore space characters, returns the number of chars ignored
  // Let first non-space in peeked
  def ignoreSpace(): Int =
  {
    try
    {
      val c = peek()
      if(c == '\n' || c == '\t' || c == ' ')
      {
        cleanPeek()
        ignoreSpace() + 1
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
  def charToString(x : Ch) =
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
  val numeric               = new Checker({x => x >= '0' && x <= '9'}, "0-9")
  def alphaLow              = new Checker({x => x >= 'a' && x <= 'z'}, "a-z")
  def alphaUp               = new Checker({x => x >= 'A' && x <= 'Z'}, "A-Z")
  def alpha                 = alphaLow    || alphaUp
  def alphaNumeric          = alpha       || numeric
  def parenthesis           = isChar('(') || isChar(')')
  def space                 = isChar(' ') || isChar('\n') || isChar('\t')
  def all                   = new Checker({x => true}, " all ")
  def isChar        (c: Ch) = new Checker({x => x == c}, charToString(c))
}


////////////////////////////////////////////////////////////////////////////////
//                           InputFromFile class                              //
////////////////////////////////////////////////////////////////////////////////
//
// Utility class to handle an input file


import java.io.FileInputStream
class InputFromFile(file:String) extends Input
{
  val inStream = new FileInputStream(file)

  // Main reading function
  def getChar_() =
  {
    val c = inStream.read()
    if (c== -1) throw new EndOfFile()
    Character.toChars(c)(0)       // convert Int to Char
  }
}


////////////////////////////////////////////////////////////////////////////////
//                           InputFromString class                            //
////////////////////////////////////////////////////////////////////////////////
//
// Utility class to handle an input string

class InputFromString(data:String) extends Input
{
  var pos = 0
  // Main reading function
  def getChar_() =
  {
    if (pos == data.length) throw new EndOfFile()
    pos = pos + 1
    data(pos - 1) // badly pos++ doesn't work
  }
}

