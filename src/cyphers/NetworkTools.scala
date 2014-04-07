////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Injector.scala]                                               //
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

package perso.utils

object NetworkTools
{
  // strings are decoded in UTF-8, but arrays of bytes are sent in ISO-8859-1 which is a injection 1 Byte -> 1 Char
  def hostToArray (msg: String) : Array[Byte] =
    msg.getBytes(java.nio.charset.Charset.forName("UTF-8"))
  def arrayToHost (msg: Array[Byte]) : String =
    new String(msg, java.nio.charset.Charset.forName("UTF-8"))
  def networkToArray (msg: String) : Array[Byte] =
    msg.getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
  def arrayToNetwork (msg: Array[Byte]) : String =
    new String(msg, java.nio.charset.Charset.forName("ISO-8859-1"))
  
  
  // encode an array of byte in a way to decode a concatenation injectively
  def injectiveString(s: Array[Byte]): Array[Byte] =
    networkToArray(s.length.toString) ++ Array('#'.toByte) ++ s

  // and decode it : return the head and the tail
  def getString(m: Array[Byte]): (Array[Byte], Array[Byte]) =
  {
    val p = m.indexOf('#')
    if(p < 0)
      throw new RuntimeException("# unfound in array of bytes")
    val len = arrayToNetwork(m.slice(0, p)).toInt
    (m.slice(p+1, p+len+1), m.slice(p+len+1, m.length))
  }
  
  // the same with a pair
  def injectivePair(s: (Array[Byte], Array[Byte])): Array[Byte] =
  {
    injectiveString(s._1) ++ injectiveString(s._2)
  }
  
  // decode a pair
  def getPair(m: Array[Byte]): (Array[Byte], Array[Byte], Array[Byte]) =
  {
    val (e1, tmp) = getString(m)
    val (e2, next) = getString(tmp)
    (e1, e2, next)
  }
}
