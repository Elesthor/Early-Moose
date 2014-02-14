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
  // Ignore the end of the current line
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
  // Ignore space characters, returns the number of ' ' ignored
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
  def Space         (x: Ch) = { x == ' ' || x == '\n' || x == '\t'}
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

  // Get a full word until a delimiter, using only char from expected.
  // Let the delimiter in peeked. End on EOF
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


  def CheckNextWord(word: String):Unit = {
    if (word != "")
    {
      val c = GetCharPeekable(All)
      if(c == word(0))
        CheckNextWord(word.reverse.dropRight(1).reverse) // TODO : abominable
      else
        throw Unexpected(c, IsChar(word(0)))
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
      case EndOfFile() => true
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
  def GetChar_() =
  {
    val c = inStream.read()
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
  case class ValueExpected() extends Exception

  // return a value nested in a TValue, or throw a ValueExpected
  def InTValue(t: Term): Value =
  {
    t match
    {
      case TValue(v) => v
      case _ => throw new ValueExpected()
    }
  }
  
  // parse a proc
  def ParseMetaProc(): MetaProc =
  {
    val left = ParseProcess()
    if(src.CheckEOF())
      new MetaProc(left, 1, None)
    else
    {
      //src.IgnoreSpace()
      val c = src.GetCharPeekable({ x:Char => x == '|' || x == '^'})
      if(c == '|')
      {
        src.CheckNextWord("|")
        new MetaProc(left, 1, Some(ParseMetaProc()))
      }
      else // c == '^'
      {
        val k = src.GetNumber()
        //src.IgnoreSpace()
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


  // parse a name
  def ParseName(delimiters: src.Checker) =
  {
    src.GetCharPeekable(src.Alpha) + src.GetWord({x: Char => src.Alpha(x) || src.Numeric(x) || x == '-' || x == '_'}, delimiters)
  }
  def ParseVariable(delimiters: src.Checker) =
  {
    new TVar(ParseName(delimiters))
  }

  def ParseChannel(delimiters: src.Checker) =
  {
    new Channel(ParseName(delimiters))
  }

  def ParseConstant() =
  {
    new VConst(ParseName(src.All))
  }

  def ParseProcessSeq(): Process =
  {
    //src.IgnoreSpace()
    if(src.CheckEOF() || src.Peek != '.')
      new PTrivial()
    else // séquence
    {
      src.CleanPeek()
      ParseProcess()
    }
  }
  
  // check if got is expected, else ignore space and check
  def CheckAfterSpace(got: Char, expected: src.Checker): Char =
  {
    if(expected(got))
      got
    else
    {
      src.IgnoreSpace()
      src.GetCharPeekable(expected)
    }
  }
  def ParseProcess(): Process =
  {
    val keyword = src.GetWord(src.Alpha, {x: Char => src.Parenthesis(x) || src.Space(x) || x == '^' || x == '0'})
    val peeked =
      /*if(keyword == "if" || keyword == "new") // keyword with a space following
      {
        if(src.IgnoreSpace() > 0)
          ' '
        else
          throw new src.Unexpected(src.Peek(), src.IsChar(' '))
      }
      else
      {
        src.IgnoreSpace()
        src.Peek()
      }*/
      src.Peek()
    src.CleanPeek()
    (keyword, peeked) match
    {
      case ("in", '(') =>
        val channel = ParseChannel(src.IsChar(','))
        src.CleanPeek()

        val variable = ParseVariable(src.IsChar(')'))
        src.CleanPeek()
        new PIn(channel, variable, ParseProcessSeq())

      case ("in", '^') =>
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
        val value = InTValue(ParseTerm())
        src.CheckNextWord(" then ")
        val P1 = ParseProcess()
        src.CheckNextWord(" else ")
        new PIf(value, P1, ParseProcess(), new PTrivial())

      case ("new", ' ') =>
        new PNew(ParseConstant(), ParseProcessSeq())
      case ("", '0') => new PTrivial
      case ("", '(') => // sous processus
        val r = ParseProcess()
        src.CheckNextWord(")")
        val n = ParseProcessSeq()
        new PSeq(r, n)
      case (_, _) => throw new SyntaxError()
    }
  }

  def ParseList(): List[Term] =
  {
    if(src.Peek() == '[')
    {
      src.CleanPeek()
      src.CheckNextWord("]")
      List()
    }
    else
    {
      val head = ParseTerm(true)
      src.CheckNextWord("::")
      head :: ParseList()
    }
  }

  def ParseTerm(inList: Boolean = false): Term =
  {
    val c = src.Peek()
    val leftTerm:Term =
      if(src.Numeric(c))
        new TValue(new VInt(src.GetNumber()))
      else
      {
        val keyword = src.GetWord({ x: Char => src.Alpha(x) || src.Numeric(x)},
                                  { x: Char => src.Parenthesis(x) || x == ' ' || x == '[' || x == ':' || x == '>' || x == '=' || x == '/' || x == '\\' || x == ' ' || x == ','})
        val peeked = src.Peek()

        if(peeked == ' ' || peeked == ',' || peeked == ')') // une variable avant un espace dans un if then else ou un variable en argument : on laisse le caractère délimiteur
          new TValue(new VConst(keyword))
        else if(peeked == ':') // list
        {
          if(inList) // an element
            return new TVar(keyword)
          else // new list
          {
            src.CleanPeek()
            src.CheckNextWord(":")
            return new ListTerm(new TVar(keyword) :: ParseList())
          }
        }
        else
        {
          src.CleanPeek()
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
              new TPk(InTValue(v))
            case ("sk", '(') =>
              val v = ParseTerm()
              src.CheckNextWord(")")
              new TSk(InTValue(v))

            // empty list
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
              new TValue(new VNot(InTValue(v)))

            // on sait que le premier caractère est une lettre (pour la condition sur les identifiants)
            case (left, '/') => // and avec une constante
              src.CheckNextWord("\\")
              return new TValue(new VAnd(new VConst(left), InTValue(ParseTerm())))
            case (left, '\\') => // or avec une constante
              src.CheckNextWord("/")
              return new TValue(new VOr(new VConst(left), InTValue(ParseTerm())))
            case (left, '=') => // = avec une variable
              return new TValue(new VEqual(new TVar(left), ParseTerm()))
            case (left, '>') => // > avec une constante
              return new TValue(new VSup(new VConst(left), InTValue(ParseTerm())))

            // sous terme
            case ("", '(') =>
              val r = ParseTerm()
              src.CheckNextWord(")")
              r
            case (_, _) => throw new SyntaxError()
          }
        }
      }

    // si le caractère suivant est un opérateur binaire
    val next = src.Peek()
    next match
    {
      case ':'  =>
        if(inList) // an element
          leftTerm
        else // new list
        {
          src.CleanPeek()
          src.CheckNextWord(":")
          new ListTerm(leftTerm :: ParseList())
        }
      case '/'  =>
        src.CleanPeek()
        src.CheckNextWord("\\")
        new TValue(new VAnd(InTValue(leftTerm), InTValue(ParseTerm())))
      case '\\' =>
        src.CleanPeek()
        src.CheckNextWord("/")
        new TValue(new VOr(InTValue(leftTerm), InTValue(ParseTerm())))
      case '='  =>
        src.CleanPeek()
        new TValue(new VEqual(leftTerm, ParseTerm()))
      case '>'  =>
        src.CleanPeek()
        new TValue(new VSup(InTValue(leftTerm), InTValue(ParseTerm())))
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
      val program = p.ParseMetaProc()
      println("end of parsing")

      println(program .RetString(0))
    }
    catch
    {
      case p.SyntaxError()       => println("Syntax Error (line " + src.line + "; col " + src.col + ")\n")
      case p.ValueExpected()     => println("A value was expected (line " + src.line + "; col " + src.col + ")\n")
      //case src.EndOfFile()       => println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
      case src.Unexpected(c, f)  =>
        print("Character ")
        if(c == '\n') print("eol")
        else print("'" + c + "'")
        print(" unexpected (line " + src.line + "; col " + src.col + ")\nExpected : ")
        for(i <- 0 to 255)
        {
          val c = Character.toChars(i)(0)
          if(f(c)) print(c)
        }
        println
    }
  }
}
