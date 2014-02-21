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
//                                Parser class                                //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the parser

import scala.collection.mutable

class Parser(src: Input)
{
  case class SyntaxError() extends Exception
  case class ValueExpected() extends Exception

  // UTILITIES
  // Return a value nested in a TValue, or throw a ValueExpected
  def InTValue(t: Term): Value =
  {
    t match
    {
      case TValue(v) => v
      case _ => throw new ValueExpected()
    }
  }
  
  // Parse a well-formed name
  def ParseName() =
  {
    // fist letter : alphabetic then alphanumeric and '-' '_'
    src.GetCharPeekable(src.Alpha) + src.GetWord(src.Alpha || src.Numeric || src.IsChar('-') || src.IsChar('_'), src.All)
  }
  
  // Check if a name is well-formed
  def CheckName(word: String) =
  {
    def it(w:String):Boolean =
    {
      word.length == 0 ||
      (
        (src.Alpha || src.Numeric || src.IsChar('-') || src.IsChar('_')) (w(0))
        && it(word.reverse.dropRight(1).reverse) // TODO : abominable
      )
    }
    // not empty, first : alphabetic, then alphanumeric ans '-' '_'
    word.length > 0 && src.Alpha(word(0)) && it(word.reverse.dropRight(1).reverse) // TODO : abominable
  }
  
  // PARSERS
  // Parse a program : call ParseProcess between || and manage ^k
  def ParseMetaProc(): MetaProc =
  {
    // parse a process
    val left = ParseProcess()
    if(src.CheckEOF()) // last
      new MetaProc(left, 1, None)
    else
    {
      //src.IgnoreSpace()
      val c = src.GetCharPeekable(src.IsChar('|') || src.IsChar('^'))
      // next :
      if(c == '|')
      {
        src.CheckNextWord("|")
        new MetaProc(left, 1, Some(ParseMetaProc()))
      }
      // ^k :
      else
      {
        val k = src.GetNumber()
        //src.IgnoreSpace()
        if(src.CheckEOF()) // last
          new MetaProc(left, k, None)
        else
        {
          // next :
          src.CheckNextWord("||")
          new MetaProc(left, k, Some(ParseMetaProc()))
        }
      }
    }
  }

  // Parse a variable
  def ParseVariable() =
  {
    new TVar(ParseName())
  }

  // Parse a channel
  // set of channels
  var channels = Map[String, Channel]()
  def ParseChannel() =
  {
    val name = ParseName()
    channels.get(name) match
    {
      case None =>
        val c = new Channel(name)
        channels += (name -> c)
        c
      case Some(c) => c
    }
  }

  // Parse a constant
  def ParseConstant() =
  {
    new VConst(ParseName())
  }
  
  // Parse the gap between two processes
  // if possible consume '.' and parse a new process
  // else return a PTrivial
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
  
  /*
  // Check if got is expected, else ignore space and check
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
  */
  
  // Parse a process
  def ParseProcess(): Process =
  {
    val keyword = src.GetWord(src.Alpha, src.Parenthesis || src.Space || src.IsChar('^') || src.IsChar('0'))
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
    // TODO : consumer les espaces et matcher aussi sur le nombre d'espaces consumé ?
    src.CleanPeek()
    (keyword, peeked) match
    {
      case ("in", '(') =>
        val channel = ParseChannel()
        src.CheckNextWord(",")

        val variable = ParseVariable()
        src.CheckNextWord(")")
        new PIn(channel, variable, ParseProcessSeq())

      case ("in", '^') =>
        val k = src.GetNumber()
        src.CheckNextWord("(")

        val c = ParseChannel()
        src.CheckNextWord(",")

        val x = ParseVariable()
        src.CheckNextWord("->")

        val u = ParseTerm()
        src.CheckNextWord(" as ")

        val y = ParseVariable()
        src.CheckNextWord(")")

        new PInk(c, x, u, y, k, ParseProcessSeq())

      case ("out", '(') =>
        val channel = ParseChannel()
        src.CheckNextWord(",")

        val message = ParseTerm()
        src.CheckNextWord(")")
        new POut(channel, message, ParseProcessSeq())

      case ("if", ' ') =>
        val value = InTValue(ParseTerm())
        src.CheckNextWord(" then ")

        val pif = ParseProcess()
        src.CheckNextWord(" else ")

        new PIf(value, pif, ParseProcess())

      case ("new", ' ') =>
        new PNew(ParseConstant(), ParseProcessSeq())

      case ("", '0') => new PTrivial
      
      case ("", '(') => // processus parenthésé
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
      if(src.Numeric(c)) // a number
        new TValue(new VInt(src.GetNumber()))
      else
      {
        val keyword = src.GetWord(src.Alpha || src.Numeric || src.IsChar('-') || src.IsChar('_'),
                                  src.Parenthesis || src.IsChar(' ') || src.IsChar('[') || src.IsChar(':') || src.IsChar('>') || src.IsChar('=') || src.IsChar('/') || src.IsChar('\\') || src.IsChar(' ') || src.IsChar(','))
        val peeked = src.Peek()

        // a variable/constant : let the delimiter
        if(peeked == ' ' || peeked == ',' || peeked == ')')
          new TValue(new VConst(keyword)) // TODO : check if keyword is well-formed

        // a variable/constant in a list, RETURN the term
        else if(peeked == ':')
        {
          val el = new TVar(keyword) // TODO : check if keyword is well-formed
          if(inList) // already in a list
            return el
          else // new list
          {
            src.CleanPeek()
            src.CheckNextWord(":")
            return new ListTerm(el :: ParseList())
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

            // operator with variable/constant : RETURN the term
            // TODO : check if keyword is well-formed
            case (left, '/') =>
              src.CheckNextWord("\\")
              return new TValue(new VAnd(new VConst(left), InTValue(ParseTerm())))

            case (left, '\\') =>
              src.CheckNextWord("/")
              return new TValue(new VOr(new VConst(left), InTValue(ParseTerm())))

            case (left, '=') =>
              return new TValue(new VEqual(new TVar(left), ParseTerm()))

            case (left, '>') =>
              return new TValue(new VSup(new VConst(left), InTValue(ParseTerm())))

            // term between parentheses
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
      case src.EndOfFile()       => println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
      case src.Unexpected(c, f)  =>
        println("Character '" + src.CharToString(c) + "' unexpected (line " + src.line + "; col " + src.col + ")\nExpected : " + f.serialized)
    }
  }
}
