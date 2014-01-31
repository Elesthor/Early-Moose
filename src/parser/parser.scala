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
  def GetChar() : Int
  def GetChar(expected:Array[Int]) : Int
  def GetWord(expected:Array[Int], delimiters:Array[Int]) : String
  var line : Int
  var col : Int
  case class Unexpected(c:Int, expected:Array[Int]) extends Exception
}

import java.io.FileInputStream
class FromFile(file:String) extends Input
{
  val inStream = new FileInputStream(file)
  var line = 0
  var col = 0
  
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
  
  def GetChar(expected:Array[Int]) =
  {
    val c = GetChar();
    if(expected.contains(c))
      c
    else
      throw new Unexpected(c, expected);
  }
  
  def GetWord(expected:Array[Int], delimiters:Array[Int]) =
  {
    var c = 0
    var word = ""
    val allExpected = expected++delimiters
    var ok = true
    while(ok)
    {
      c = GetChar(allExpected)
      if(expected.contains(c))
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

