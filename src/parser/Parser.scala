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
import perso.utils.NetworkTools._


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
  case class NameMalformed(name: String)    extends Exception

////////////////////////////////////////////////////////////////////////////////
//                          Utilities function                                //
////////////////////////////////////////////////////////////////////////////////

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

    word match
    {
      case "in" | "as" | "out" | "if" | "then" | "else" | "new"
      | "pair" | "pi1" | "pi2" | "enc" | "dec" | "pk" | "sk" | "count"
      | "not" | "openEnc" =>
        throw new NameMalformed(word)
      case _ =>
        // not empty and first char is alphabetic
        // and then alphanumeric or '-' '_'
        if(word.length == 0 || !src.alpha(word(0)) || !it(word.drop(1)))
          throw new NameMalformed(word)
    }
  }
  
  // Replace the last PTrivial in an AST with pro
  def replacePTrivial(in: Process, pro: Process): Process =
  {
    in match
    {
      case PTrivial() => pro
      case PIn(c, v, p) => new PIn(c, v, replacePTrivial(p, pro))
      case PInk(c, v, u, y, k, p) => new PInk(c, v, u, y, k, replacePTrivial(p, pro))
      case POut(c, t, p) => new POut(c, t, replacePTrivial(p, pro))
      case PIf(v, pIf, pElse, p) => new PIf(v, pIf, pElse, replacePTrivial(p, pro))
      case PNew(s, p) => new PNew(s, replacePTrivial(p, pro))
    }
  }

  // check if we have ) then EOF, useful for player
  def checkEnd():Boolean =
  {
    try
    {
      src.checkNextWord(")") 
      src.checkEOF() 
    } catch
    {
      case src.Unexpected(_, _) => false
    }
  }

////////////////////////////////////////////////////////////////////////////////
//                               Parse Name                                   //
////////////////////////////////////////////////////////////////////////////////
  def parseName() =
  {
    val name = src.getWord(src.alphaNumeric || src.isChar('-') || src.isChar('_'), src.all)
    checkName(name)
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
  def parseChannel(): String =
  {
    parseName()
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
//                             Parse . at end of a process                    //
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
        new TValue(new VInt(src.getNumber().toLong))
      else
      {
        val keyword = src.getWord(src.alphaNumeric || src.isChar('-') || src.isChar('_'),
                                  src.parenthesis || src.space || src.isChar('[') || src.isChar(':') || src.isChar('>') || src.isChar('=') || src.isChar('/') || src.isChar('\\') || src.isChar(','))
        val spaces = src.ignoreSpace()
        val peeked = src.peek()

        (keyword, peeked) match
        {
          case ("pair", '(') =>
            src.cleanPeek()
            val left = parseTerm()
            src.checkNextWord(",")
            val right = parseTerm()
            src.checkNextWord(")")
            new TPair(left, right)

          case ("pi1", '(') =>
            src.cleanPeek()
            val t = parseTerm()
            src.checkNextWord(")")

            new TPi1(t)

          case ("pi2", '(') =>
            src.cleanPeek()
            val t = parseTerm()
            src.checkNextWord(")")
            new TPi2(t)

          case ("enc", '(') =>
            src.cleanPeek()
            val left = parseTerm()
            src.checkNextWord(",")
            val right = parseTerm()
            src.checkNextWord(",")
            val seed = parseTerm()
            src.checkNextWord(")")
            new TEnc(left, right, seed)

          case ("dec", '(') =>
            src.cleanPeek()
            val left = parseTerm()
            src.checkNextWord(",")
            val right = parseTerm()
            src.checkNextWord(")")
            new TDec(left, right)

          case ("pk", '(') =>
            src.cleanPeek()
            val v = parseTerm()
            src.ignoreSpace()
            src.peek() match
            {
              case ',' =>
                src.cleanPeek()
                val crypto = src.getWord(src.alphaNumeric, src.parenthesis)
                src.checkNextWord(")")
                new TPk(v, crypto)
              case ')' =>
                src.cleanPeek()
                new TPk(v, "default")
            }

          case ("sk", '(') =>
            src.cleanPeek()
            val v = parseTerm()
            src.ignoreSpace()
            src.peek() match
            {
              case ',' =>
                src.cleanPeek()
                val crypto = src.getWord(src.alphaNumeric, src.parenthesis)
                src.checkNextWord(")")
                new TSk(v, crypto)
              case ')' =>
                src.cleanPeek()
                new TSk(v, "default")
            }

          
          case ("raw", '(') =>
            src.cleanPeek()
            val data = src.getRaw()
            src.checkNextWord(")")
            new TRaw(arrayToHost(networkToArray(data)))
          
          case ("openEnc", '(') => // TODO cryptosys
            src.cleanPeek()
            val v = parseTerm()
            src.checkNextWord(")")
            new TOpenEnc(v)

          // empty list
          case ("", '[') =>
            src.cleanPeek()
            src.checkNextWord("]")
            new ListTerm(List())

          // Values
          case ("count", '(') =>
            src.cleanPeek()
            val l = parseTerm()
            src.checkNextWord(")")
            new TValue(new VCount(l))

          case ("not", '(') =>
            src.cleanPeek()
            val v = parseTerm()
            src.checkNextWord(")")
            new TValue(new VNot(v))

          // term between parentheses
          case ("", '(') =>
            src.cleanPeek()
            val r = parseTerm()
            src.checkNextWord(")")
            r

          // variable
          case (name, _)    =>
            checkName(name) // well-formed
            new TVar(name)
        }
      }

    // if next char is a binary operator
    src.ignoreSpace()
    if(src.checkEOF())
      leftTerm
    else
      src.peek() match
      {
        case ':'  =>
          if(inList) // an element, let the delimiter
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
    val keyword = src.getWord(src.alpha, src.parenthesis || src.space || src.isChar('^') || src.isChar('0') || src.isChar('='))
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
        val k = src.getNumber().toInt  
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

      case ("connect", '(') =>
        src.cleanPeek()
        val channel = parseChannel()
        src.checkNextWord(",")

        val host = src.getWord(src.alphaNumeric || src.isChar('-') || src.isChar('_') || src.isChar('.'), src.all)
        src.checkNextWord(",")
        
        val port = src.getNumber().toInt
        src.checkNextWord(")")
        new PConnect(channel, host, port, parseProcessSeq())

      case ("accept", '(') =>
        src.cleanPeek()
        val channel = parseChannel()
        src.checkNextWord(",")
        
        val port = src.getNumber().toInt
        src.checkNextWord(")")
        new PAccept(channel, port, parseProcessSeq())
      
      case ("wait", '(') =>
        src.cleanPeek()
        val channel = parseChannel()
        src.checkNextWord(")")
        new PWait(channel, parseProcessSeq())
      
      case ("close", '(') =>
        src.cleanPeek()
        val channel = parseChannel()
        src.checkNextWord(")")
        new PClose(channel, parseProcessSeq())

      case ("if", p) =>
        if(spaces == 0) throw new src.Unexpected(p, src.space)

        val value = parseTerm()
        src.checkNextWord("then")

        val pif = parseProcess()
        src.checkNextWord("else")

        new PIf(value, pif, parseProcess(), new PTrivial()) // PTrivial may be
        // replaced by replacePTrivial

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
        replacePTrivial(r, n)
      case (name, '=') => // affectation
        src.cleanPeek()
        checkName(name)
        new PAff(name, parseTerm(), parseProcessSeq())
        
      case _ => throw new SyntaxError()
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
        val k = src.getNumber().toInt
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

