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


import scala.collection.mutable


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                Parser class                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// Implementation of the parser

class Parser(src: Input)
{

  // Definition of errors
  case class SyntaxError()                  extends Exception
  case class ValueExpected()                extends Exception
  case class NameMalformed(name: String)    extends Exception

////////////////////////////////////////////////////////////////////////////////
//                          Utilities function                                //
////////////////////////////////////////////////////////////////////////////////

  // Return a value nested in a TValue, or throw a ValueExpected
  def inTValue(t: Term): Value =
  {
    t match
    {
      case TValue(v) => v
      case TVar(v) => new VConst(v)
      case _ => throw new ValueExpected()
    }
  }

  // Check if a name is well-formed (throw NameMalformed)
  def checkName(word: String) =
  {
    def it(w:String):Boolean =
    {
      w.length == 0 ||
      (
        (src.alphaNumeric || src.isChar('-') || src.isChar('_')) (w(0))
        && it(w.drop(1))
      )
    }
    // not empty and first char is alphabetic, and then alphanumeric or '-' '_'
    if(word.length == 0 || !src.alpha(word(0)) || !it(word.drop(1)))
      throw new NameMalformed(word)
  }

////////////////////////////////////////////////////////////////////////////////
//                               Parse Name                                   //
////////////////////////////////////////////////////////////////////////////////
  def parseName() =
  {
    // non empty and first char is alphabetic, and then alphanumeric or '-' '_'
    val name = src.getWord(src.alphaNumeric || src.isChar('-') || src.isChar('_'), src.all)
    if(name.length == 0 || !src.alpha(name(0))) throw new NameMalformed(name)
    name
  }

////////////////////////////////////////////////////////////////////////////////
//                            Parse Variable                                  //
////////////////////////////////////////////////////////////////////////////////
  def parseVariable():TVar =
  {
    src.ignoreSpace()
    src.peek() match
    {
      case '(' =>
        src.cleanPeek()
        val v = parseVariable()
        src.checkNextWord(")")
        return v
      case _   =>
        new TVar(parseName())
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse Channels                                 //
////////////////////////////////////////////////////////////////////////////////
  var channels = Map[String, Channel]()
  def parseChannel(): Channel =
  {
    val name = parseName()
    channels.get(name) match
    {
      case None =>
        val c = new Channel(name)
        channels += (name -> c)
        return c
      case Some(c) => c
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse Constants                                //
////////////////////////////////////////////////////////////////////////////////
  def parseConstant(): VConst =
  {
    src.ignoreSpace()
    src.peek() match
    {
      case '(' =>
        src.cleanPeek()
        val v = parseConstant()
        src.checkNextWord(")")
        return v
      case _   =>
        new VConst(parseName())
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse PSeq                                     //
////////////////////////////////////////////////////////////////////////////////
// Parse the gap between two processes
// if possible, consume '.' and parse a new process
// else return a PTrivial
  def parseProcessSeq(): Process =
  {
    src.ignoreSpace()
    if(src.checkEOF() || src.peek() != '.')
      new PTrivial()
    else // séquence
    {
      src.cleanPeek()
      parseProcess()
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse List                                     //
////////////////////////////////////////////////////////////////////////////////
  def parseList(): List[Term] =
  {
    src.ignoreSpace()
    if(src.peek() == '[')
    {
      src.cleanPeek()
      src.checkNextWord("]")
      List()
    }
    else
    {
      val head = parseTerm(true)
      src.checkNextWord("::")
      head :: parseList()
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse Term                                     //
////////////////////////////////////////////////////////////////////////////////
  def parseTerm(inList: Boolean = false): Term =
  {
    src.ignoreSpace()
    val c = src.peek()
    val leftTerm:Term =
      if(src.numeric(c) || src.isChar('-')(c)) // a number
        new TValue(new VInt(src.getNumber()))
      else
      {
        val keyword = src.getWord(src.alphaNumeric || src.isChar('-') || src.isChar('_'),
                                  src.parenthesis || src.space || src.isChar('[') || src.isChar(':') || src.isChar('>') || src.isChar('=') || src.isChar('/') || src.isChar('\\') || src.isChar(','))
        val spaces = src.ignoreSpace()
        val peeked = src.peek()

        // a variable/constant : let the delimiter
        if(peeked == ',' || peeked == ')')
        {
          checkName(keyword)
          new TVar(keyword)
        }

        // a variable/constant in a list, RETURN the term
        else if(peeked == ':')
        {
          checkName(keyword)
          val el = new TVar(keyword)
          if(inList) // already in a list
            return el
          else // new list
          {
            src.cleanPeek()
            src.checkNextWord(":")
            return new ListTerm(el :: parseList())
          }
        }
        else
        {
          (keyword, peeked) match
          {
            case ("pair", d) => // generic on d : avoid matching (name, _) with name="pair"
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val left = parseTerm()
              src.checkNextWord(",")
              val right = parseTerm()
              src.checkNextWord(")")
              new TPair(left, right)

            case ("pi1", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val t = parseTerm()
              src.checkNextWord(")")

              new TPi1(t)

            case ("pi2", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val t = parseTerm()
              src.checkNextWord(")")
              new TPi2(t)

            case ("enc", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val left = parseTerm()
              src.checkNextWord(",")
              val right = parseTerm()
              src.checkNextWord(")")
              new TEnc(left, right)

            case ("dec", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val left = parseTerm()
              src.checkNextWord(",")
              val right = parseTerm()
              src.checkNextWord(")")
              new TDec(left, right)

            case ("pk", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val v = parseTerm()
              src.checkNextWord(")")
              new TPk(v)

            case ("sk", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val v = parseTerm()
              src.checkNextWord(")")
              new TSk(v)

            // empty list
            case ("", '[') =>
              src.cleanPeek()
              src.checkNextWord("]")
              new ListTerm(List())

            // Values
            case ("count", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val l = parseTerm()
              src.checkNextWord(")")
              new TValue(new VCount(l))

            case ("not", d) =>
              if(d != '(') throw new src.Unexpected(d, src.isChar('('))
              src.cleanPeek()
              val v = parseTerm()
              src.checkNextWord(")")
              new TValue(new VNot(v))

            // operator with variable/constant : RETURN the term
            case (left, '/') =>
              src.cleanPeek()
              src.checkNextWord("\\")
              checkName(left)
              return new TValue(new VAnd(new TVar(left), parseTerm()))

            case (left, '\\') =>
              src.cleanPeek()
              src.checkNextWord("/")
              checkName(left)
              return new TValue(new VOr(new TVar(left), parseTerm()))

            case (left, '=') =>
              src.cleanPeek()
              checkName(left)
              return new TValue(new VEqual(new TVar(left), parseTerm()))

            case (left, '>') =>
              src.cleanPeek()
              checkName(left)
              return new TValue(new VSup(new TVar(left), parseTerm()))

            // term between parentheses
            case ("", '(') =>
              src.cleanPeek()
              val r = parseTerm()
              src.checkNextWord(")")
              r

            case (name, _) =>
              return new TVar(name)
          }
        }
      }

    // si le caractère suivant est un opérateur binaire
    src.ignoreSpace()
    src.peek() match
    {
      case ':'  =>
        if(inList) // an element
          leftTerm
        else // new list
        {
          src.cleanPeek()
          src.checkNextWord(":")
          new ListTerm(leftTerm :: parseList())
        }
      case '/'  =>
        src.cleanPeek()
        src.checkNextWord("\\")
        new TValue(new VAnd(leftTerm, parseTerm()))
      case '\\' =>
        src.cleanPeek()
        src.checkNextWord("/")
        new TValue(new VOr(leftTerm, parseTerm()))
      case '='  =>
        src.cleanPeek()
        new TValue(new VEqual(leftTerm, parseTerm()))
      case '>'  =>
        src.cleanPeek()
        new TValue(new VSup(leftTerm, parseTerm()))
      case _    => leftTerm
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse Process                                  //
////////////////////////////////////////////////////////////////////////////////
  def parseProcess(): Process =
  {
    val keyword = src.getWord(src.alpha, src.parenthesis || src.space || src.isChar('^') || src.isChar('0'))
    val spaces = src.ignoreSpace()
    val peeked = src.peek()

    (keyword, peeked) match
    {
      case ("in", '(') =>
        src.cleanPeek()
        val channel = parseChannel()
        src.checkNextWord(",")

        val variable = parseVariable()
        src.checkNextWord(")")
        new PIn(channel, variable, parseProcessSeq())

      case ("in", '^') =>
        src.cleanPeek()
        val k = src.getNumber()
        src.checkNextWord("(")

        val c = parseChannel()
        src.checkNextWord(",")

        val x = parseVariable()
        src.checkNextWord("->")

        val u = parseTerm()
        src.checkNextWord("as")

        val y = parseVariable()
        src.checkNextWord(")")

        new PInk(c, x, u, y, k, parseProcessSeq())

      case ("out", '(') =>
        src.cleanPeek()
        val channel = parseChannel()
        src.checkNextWord(",")

        val message = parseTerm()
        src.checkNextWord(")")
        new POut(channel, message, parseProcessSeq())

      case ("if", p) =>
        if(spaces == 0) throw new src.Unexpected(p, src.space)

        val value = parseTerm()
        src.checkNextWord("then")

        val pif = parseProcess()
        src.checkNextWord("else")

        new PIf(value, pif, parseProcess())

      case ("new", p) =>
        if(spaces == 0) throw new src.Unexpected(p, src.space)

        new PNew(parseConstant(), parseProcessSeq())

      case ("", '0') =>
        src.cleanPeek()
        new PTrivial()

      case ("", '(') => // process between parentheses
        src.cleanPeek()
        val r = parseProcess()
        src.checkNextWord(")")

        val n = parseProcessSeq()
        new PSeq(r, n)
      case (_, _) => throw new SyntaxError()
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Parse MetaProc                                 //
////////////////////////////////////////////////////////////////////////////////
// Parse a program : call parseProcess between || and manage ^k
  def parseMetaProc(): MetaProc =
  {
    // parse a process
    val left = parseProcess()
    src.ignoreSpace()
    if(src.checkEOF()) // last
      new MetaProc(left, 1, None)
    else
    {
      val c = src.getCharPeekable(src.isChar('|') || src.isChar('^'))
      // next :
      if(c == '|')
      {
        src.checkNextWord("|")
        new MetaProc(left, 1, Some(parseMetaProc()))
      }
      // ^k :
      else
      {
        val k = src.getNumber()
        src.ignoreSpace()
        if(src.checkEOF()) // last
          new MetaProc(left, k, None)
        else
        {
          // next :
          src.checkNextWord("||")
          new MetaProc(left, k, Some(parseMetaProc()))
        }
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                             Calling function                               //
////////////////////////////////////////////////////////////////////////////////
  def parse() =
  {
    parseMetaProc()
  }
}

