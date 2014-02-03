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
  def GetChar(): Ch
  
  type Ch = Char
  type Checker = Char => Boolean
  var line = 0
  var col = 0
  var peeked: Option[Ch] = None
  def Peek(expected: Checker): Ch =
  {
    val c = GetChar()
    peeked = Some(c)
    if(expected(c))
      c
    else
      throw new Unexpected(c, expected);
  }
  def GetPeeked () =
  {
    val c = peeked.get // throw an exception if peeked is None
    peeked = None
    c
  }
  def GetWord(expected: Checker, delimiters: Checker): String =
  {
    def allExpected = { x: Ch => expected(x) || delimiters(x) }
    var word = ""
    var ok = true
    while(ok)
    {
      val c = Peek(allExpected)
      if(expected(c))
        word += c
      else
        ok = false
    }
    word
  }
  def GetNumber() =
  {
    Integer.parseInt(GetWord(Numeric, {x => true}))
  }
  
  case class Unexpected(c:Ch, expected: Checker) extends Exception
  case class EndOfFile() extends Exception
  
  def Numeric  (x: Ch) = { x >= '0' && x <= '9' }
  def AlphaLow (x: Ch) = { x >= 'a' && x <= 'z' }
  def AlphaUp  (x: Ch) = { x >= 'A' && x <= 'Z' }
  def Alpha(x: Ch) = { AlphaLow(x) || AlphaUp(x) }
  def AlphaNumeric(x: Ch) = { Alpha(x) || Numeric(x) }
  def IsChar(c: Ch)(x: Ch) = { x == c }
  def Parenthesis(x: Ch) = { x == '(' || x == ')' }
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

class Parser(src: Input)
{
  case class SyntaxError(line:Int, col:Int) extends Exception
  
  def CheckPeeked(c: src.Ch)
  {
    if(src.GetPeeked() != c)
      throw SyntaxError(src.line, src.col)
  }
  
  def ParseVariable(delimiters: src.Checker) =
  {
    new TVar(src.GetWord(src.Alpha, delimiters))
  }
  def ParseChannel() =
  {
    new Channel(src.GetNumber())
  }
  def ParseProcess(): Process =
  {
    val keyword = src.GetWord(src.Alpha, {x: Char => src.Parenthesis(x) || x == ' ' || x == '^' || x == '0'})
    val peeked = src.GetPeeked()
    (keyword, peeked) match
    {
      case ("in", '(') =>
        val channel = ParseChannel()
        CheckPeeked(',')
        
        val variable = ParseVariable(src.IsChar(')'))
        // no need to check if peeked is Some(')')
        src.Peek(src.IsChar('.'))
        
        new PIn(channel, variable, ParseProcess())
        
      case ("in", '^') =>
        new PTrivial
        /*
        val k = src.GetNumber()
        CheckPeeked('(')
        
        val channel = ParseChannel()
        CheckPeeked(',')
        
        val variable = ParseVariable(src.IsChar('-'))
        // no need to check if peeked is Some('-')
        src.Peek(src.IsChar('>'))
        */
        
        
        
        
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


