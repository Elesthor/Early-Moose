////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [parser.scala]                                                   //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//



trait Input
{
  type Ch = Char
  def GetChar() : Ch
  def numeric(x : Ch) : Boolean
  def alphaLow(x : Ch) : Boolean
  def alphaUp(x : Ch) : Boolean
  
  var line = 0
  var col = 0
  var peeked : Option[Ch] = None
  def Peek(expected: Ch => Boolean) : Ch =
  {
    val c = Peek();
    if(expected(c))
      c
    else
      throw new Unexpected(c, expected);
  }
  def Peek() =
  {
    val c = GetChar()
    peeked = Some(c)
    c
  }
  def GetPeeked () =
  {
    val c = peeked.get // throw an exception if peeked is None
    peeked = None
    c
  }
  def GetWord(expected: Ch => Boolean, delimiters: Ch => Boolean) : String =
  {
    def allExpected = { x : Ch => expected(x) || delimiters(x) }
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
    Integer.parseInt(GetWord(numeric, {x => true}))
  }
  
  case class Unexpected(c:Ch, expected: Ch => Boolean) extends Exception
  case class EndOfFile() extends Exception
  
  def alpha(x : Ch) = { alphaLow(x) || alphaUp(x) }
  def alphaNumeric(x : Ch) = { alpha(x) || numeric(x) }
  def parenthesis(x : Ch) = { x == '(' || x == ')' }
}

import java.io.FileInputStream
class InputFromFile(file:String) extends Input
{
  val inStream = new FileInputStream(file)
  
  def numeric  (x : Ch) = { x >= '0' && x <= '9' }
  def alphaLow (x : Ch) = { x >= 'a' && x <= 'z' }
  def alphaUp  (x : Ch) = { x >= 'A' && x <= 'Z' }
  
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
  def ParseProcess() =
  {
    println(src.GetWord(src.alpha, src.parenthesis))
    println(src.GetPeeked())
    println(src.GetPeeked())
  }
}

val in = new InputFromFile("test")
val p = new Parser(in)
p.ParseProcess()


