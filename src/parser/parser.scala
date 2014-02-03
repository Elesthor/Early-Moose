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
  type Ch
  def GetChar() : Ch
  def GetChar(expected: Ch => Boolean) : Ch
  def GetWord(expected: Ch => Boolean, delimiters: Ch => Boolean) : String
  var line : Int
  var col : Int
  case class Unexpected(c:Ch, expected: Ch => Boolean) extends Exception
  def numeric(x : Ch) : Boolean
  def alphaLow(x : Ch) : Boolean
  def alphaUp(x : Ch) : Boolean
  def alpha(x : Ch) = { alphaLow(x) || alphaUp(x) }
  def alphaNumeric(x : Ch) = { alpha(x) || numeric(x) }
  def parenthesis(x : Ch) = { x == '(' || x == ')' }
}

import java.io.FileInputStream
class InputFromFile(file:String) extends Input
{
  type Ch = Int
  val inStream = new FileInputStream(file)
  var line = 0
  var col = 0
  def numeric (x : Ch) =
  {
    x >= '0' && x <= '9'
  }
  def alphaLow (x : Ch) =
  {
    x >= '0' && x <= '9'
  }
  def alphaUp (x : Ch) =
  {
    x >= '0' && x <= '9'
  }
  
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
    c
  }
  
  def GetChar(expected: Ch => Boolean) =
  {
    val c = GetChar();
    if(expected(c))
      c
    else
      throw new Unexpected(c, expected);
  }
  
  def GetWord(expected: Ch => Boolean, delimiters: Ch => Boolean) =
  {
    var c = 0
    var word = ""
    def allExpected = {x : Ch => expected(x) || delimiters(x)}
    var ok = true
    while(ok)
    {
      c = GetChar(allExpected)
      if(expected(c))
        word += c
      else
        ok = false
    }
    word
  }
}

class parser(src : Input)
{
  def ParseProcess =
  {
    //var name = src.GetWord();
  }
}


