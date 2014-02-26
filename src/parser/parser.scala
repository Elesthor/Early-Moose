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
  case class NameMalformed(name: String) extends Exception

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

  // Check if a name is well-formed (throw NameMalformed)
  def CheckName(word: String) =
  {
    def it(w:String):Boolean =
    {
      w.length == 0 ||
      (
        (src.Alpha || src.Numeric || src.IsChar('-') || src.IsChar('_')) (w(0))
        && it(w.reverse.dropRight(1).reverse) // TODO : abominable
      )
    }
    // not empty and first char is alphabetic, and then alphanumeric or '-' '_'
    if(word.length == 0 || !src.Alpha(word(0)) || !it(word.reverse.dropRight(1).reverse)) // TODO : abominable
      throw new NameMalformed(word)
  }

  // PARSERS
  // Parse a well-formed name
  def ParseName() =
  {
    // non empty and first char is alphabetic, and then alphanumeric or '-' '_'
    val name = src.GetWord(src.Alpha || src.Numeric || src.IsChar('-') || src.IsChar('_'), src.All)
    if(name.length == 0 || !src.Alpha(name(0))) throw new NameMalformed(name)
    name
  }
  
  // Parse a program : call ParseProcess between || and manage ^k
  def ParseMetaProc(): MetaProc =
  {
    // parse a process
    val left = ParseProcess()
    src.IgnoreSpace()
    if(src.CheckEOF()) // last
      new MetaProc(left, 1, None)
    else
    {
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
        src.IgnoreSpace()
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
  def ParseVariable():TVar =
  {
    src.IgnoreSpace()
    src.Peek() match
    {
      case '(' =>
        src.CleanPeek()
        val v = ParseVariable()
        src.CheckNextWord(")")
        v
      case _   =>
        new TVar(ParseName())
    }
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
  def ParseConstant():VConst =
  {
    src.IgnoreSpace()
    src.Peek() match
    {
      case '(' =>
        src.CleanPeek()
        val v = ParseConstant()
        src.CheckNextWord(")")
        v
      case _   =>
        new VConst(ParseName())
    }
  }

  // Parse the gap between two processes
  // if possible, consume '.' and parse a new process
  // else return a PTrivial
  def ParseProcessSeq(): Process =
  {
    src.IgnoreSpace()
    if(src.CheckEOF() || src.Peek != '.')
      new PTrivial()
    else // séquence
    {
      src.CleanPeek()
      ParseProcess()
    }
  }

  // Parse a process
  def ParseProcess(): Process =
  {
    val keyword = src.GetWord(src.Alpha, src.Parenthesis || src.Space || src.IsChar('^') || src.IsChar('0'))
    val spaces = src.IgnoreSpace()
    val peeked = src.Peek()

    (keyword, peeked) match
    {
      case ("in", '(') =>
        src.CleanPeek()
        val channel = ParseChannel()
        src.CheckNextWord(",")

        val variable = ParseVariable()
        src.CheckNextWord(")")
        new PIn(channel, variable, ParseProcessSeq())

      case ("in", '^') =>
        src.CleanPeek()
        val k = src.GetNumber()
        src.CheckNextWord("(")

        val c = ParseChannel()
        src.CheckNextWord(",")

        val x = ParseVariable()
        src.CheckNextWord("->")

        val u = ParseTerm()
        src.CheckNextWord("as")

        val y = ParseVariable()
        src.CheckNextWord(")")

        new PInk(c, x, u, y, k, ParseProcessSeq())

      case ("out", '(') =>
        src.CleanPeek()
        val channel = ParseChannel()
        src.CheckNextWord(",")

        val message = ParseTerm()
        src.CheckNextWord(")")
        new POut(channel, message, ParseProcessSeq())

      case ("if", p) =>
        if(spaces == 0) throw new src.Unexpected(p, src.Space)

        val value = InTValue(ParseTerm())
        src.CheckNextWord("then")

        val pif = ParseProcess()
        src.CheckNextWord("else")

        new PIf(value, pif, ParseProcess())

      case ("new", p) =>
        if(spaces == 0) throw new src.Unexpected(p, src.Space)

        new PNew(ParseConstant(), ParseProcessSeq())

      case ("", '0') =>
        src.CleanPeek()
        new PTrivial

      case ("", '(') => // process between parentheses
        src.CleanPeek()
        val r = ParseProcess()
        src.CheckNextWord(")")

        val n = ParseProcessSeq()
        new PSeq(r, n)
      case (_, _) => throw new SyntaxError()
    }
  }

  def ParseList(): List[Term] =
  {
    src.IgnoreSpace()
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
    src.IgnoreSpace()
    val c = src.Peek()
    val leftTerm:Term =
      if(src.Numeric(c)) // a number
        new TValue(new VInt(src.GetNumber()))
      else
      {
        val keyword = src.GetWord(src.Alpha || src.Numeric || src.IsChar('-') || src.IsChar('_'),
                                  src.Parenthesis || src.Space || src.IsChar('[') || src.IsChar(':') || src.IsChar('>') || src.IsChar('=') || src.IsChar('/') || src.IsChar('\\') || src.IsChar(','))
        val spaces = src.IgnoreSpace()
        val peeked = src.Peek()

        // a variable/constant : let the delimiter
        if(peeked == ',' || peeked == ')')
        {
          CheckName(keyword)
          new TValue(new VConst(keyword))
        }

        // a variable/constant in a list, RETURN the term
        else if(peeked == ':')
        {
          CheckName(keyword)
          val el = new TVar(keyword)
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
          (keyword, peeked) match
          {
            case ("pair", d) => // generic on d : avoid matching (name, _) with name="pair"
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val left = ParseTerm()
              src.CheckNextWord(",")

              val right = ParseTerm()
              src.CheckNextWord(")")

              new TPair(left, right)

            case ("pi1", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val t = ParseTerm()
              src.CheckNextWord(")")

              new TPi1(t)

            case ("pi2", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val t = ParseTerm()
              src.CheckNextWord(")")

              new TPi2(t)

            case ("enc", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val left = ParseTerm()
              src.CheckNextWord(",")

              val right = ParseTerm()
              src.CheckNextWord(")")

              new TEnc(left, right)

            case ("dec", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val left = ParseTerm()
              src.CheckNextWord(",")

              val right = ParseTerm()
              src.CheckNextWord(")")

              new TDec(left, right)

            case ("pk", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val v = ParseTerm()
              src.CheckNextWord(")")

              new TPk(InTValue(v))

            case ("sk", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val v = ParseTerm()
              src.CheckNextWord(")")

              new TSk(InTValue(v))

            // empty list
            case ("", '[') =>
              src.CleanPeek()
              src.CheckNextWord("]")
              new ListTerm(List())

            // Values
            case ("count", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val l = ParseTerm()
              src.CheckNextWord(")")

              new TValue(new VCount(l))

            case ("not", d) =>
              if(d != '(') throw new src.Unexpected(d, src.IsChar('('))
              src.CleanPeek()
              val v = ParseTerm()
              src.CheckNextWord(")")

              new TValue(new VNot(InTValue(v)))

            // operator with variable/constant : RETURN the term
            case (left, '/') =>
              src.CleanPeek()
              src.CheckNextWord("\\")
              CheckName(left)
              return new TValue(new VAnd(new VConst(left), InTValue(ParseTerm())))

            case (left, '\\') =>
              src.CleanPeek()
              src.CheckNextWord("/")
              CheckName(left)
              return new TValue(new VOr(new VConst(left), InTValue(ParseTerm())))

            case (left, '=') =>
              src.CleanPeek()
              CheckName(left)
              return new TValue(new VEqual(new TVar(left), ParseTerm()))

            case (left, '>') =>
              src.CleanPeek()
              CheckName(left)
              return new TValue(new VSup(new VConst(left), InTValue(ParseTerm())))

            // term between parentheses
            case ("", '(') =>
              src.CleanPeek()
              val r = ParseTerm()
              src.CheckNextWord(")")
              r

            case (name, _) =>
              return new TVar(name)
          }
        }
      }

    // si le caractère suivant est un opérateur binaire
    src.IgnoreSpace()
    src.Peek() match
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
    //val src = new InputFromFile("test")
    val src = new InputFromString("(new v).out(c,enc(v,pk(1)))^15 || in^15(c,x -> dec(x,sk(1)) as y).out(stdout,count(y))")
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
      case p.NameMalformed(name) => println("Malformed name : '" + name + "' (line " + src.line + "; col " + src.col + ")\n")
      case src.EndOfFile()       => println("End of file unexpected (line " + src.line + "; col " + src.col + ")\n")
      case src.Unexpected(c, f)  =>
        println("Character '" + src.CharToString(c) + "' unexpected (line " + src.line + "; col " + src.col + ")\nExpected : " + f.serialized)
    }
  }
}
