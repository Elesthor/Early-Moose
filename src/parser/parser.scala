////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [parser.scala]                                                   //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//



abstract class Input
{
  type Ch = Char
  def GetChar() : Ch

  var line = 0
  var col = 0
  var peeked : Option[Ch] = None
  def Peek(expected: Ch => Boolean) : Ch =
  {
    val c = GetChar()
    peeked = Some(c)
    if(expected(c)) c else throw new Unexpected(c, expected);
  }


  def GetPeeked () =
  {
    val c = peeked.get // throw an exception if peeked is None
    peeked = None
    c
  }

  def GetWord(expected: Ch => Boolean, delimiters: Ch => Boolean) : String =
  {
    def collector(): String = {
      val c = Peek({ x : Ch => expected(x) || delimiters(x) })
      if (expected(c)) c+collector() else ""
    }
    collector()
  }


  def GetNumber() =
  {
    Integer.parseInt(GetWord(numeric, {x => true}))
  }

  case class Unexpected(c:Ch, expected: Ch => Boolean) extends Exception
  case class EndOfFile() extends Exception

  def numeric  (x : Ch) = { x >= '0' && x <= '9' }
  def alphaLow (x : Ch) = { x >= 'a' && x <= 'z' }
  def alphaUp  (x : Ch) = { x >= 'A' && x <= 'Z' }
  def alpha(x : Ch) = { alphaLow(x) || alphaUp(x) }
  def alphaNumeric(x : Ch) = { alpha(x) || numeric(x) }
  def parenthesis(x : Ch) = { x == '(' || x == ')' }
}

import java.io.FileInputStream
class InputFromFile(file:String) extends Input
{
  val inStream = new FileInputStream(file)

  def GetChar() =
  {
    val c = inStream.read()
    if(c == '\n')
    {
      line = line + 1;
      col = 0
    }
    else
      col = col + 1
    if(c== -1)
      throw EndOfFile()

    Character.toChars(c)(0) // convert Int to Char
  }
}

class Parser(src : Input)
{
  case class SyntaxError(line:Int, col:Int) extends Exception

  def CheckPeeked(c : Char)
  {
    if(src.GetPeeked() != c)
      throw SyntaxError(src.line, src.col)
  }

  def ParseProcess() : Process =
  {
    val keyword = src.GetWord(src.alpha, {x : Char => src.parenthesis(x) || x == ' ' || x == '^' || x == '0'})
    val peeked = src.GetPeeked()
    (keyword, peeked) match
    {
      case ("in", '(') =>
        val channel = new Channel(src.GetNumber())
        CheckPeeked(',')

        val variable = new TVar(src.GetWord(src.alpha, { x : Char => x == ')' }))
        // no need to check if peeked = Some(')')
        src.Peek()
        CheckPeeked('.')

        new PIn(channel, variable, ParseProcess())

      case ("in", '^') => new PTrivial
      case ("out", '(') => new PTrivial
      case ("if", ' ') => new PTrivial
      case ("new", ' ') => new PTrivial
      case ("", '0') => new PTrivial
      case (_, _) => throw SyntaxError(src.line, src.col)
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

