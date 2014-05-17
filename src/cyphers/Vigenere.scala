////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Vigenere.scala]                                               //
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Vigenere Key                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class VigenereKey(seed: Long) extends Key[Array[Byte]]
{
  def generate() : Array[Byte] =
  {
    // random Array[Byte] of random size
    val randomizer = new util.Random(seed)
    // nextString return string of any character (chinese...), we cast them to bytes
    randomizer.nextString(randomizer.nextInt() % 64 + 64).toCharArray.map(_.toByte)
  }
  
  def getPublic  = generate()
  def getPrivate = getPublic.map({x => (-x).toByte})
  
  def getString(k: Array[Byte]): String =
    new String(k, java.nio.charset.Charset.forName("ISO-8859-1"))
  def fromString(s: String): Array[Byte] =
    s.getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            Vigenere Implementation                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Vigenere extends CryptoSystem [Array[Byte]]
{
  def _encrypt (msg: Array[Byte], key: Array[Byte], _seed:Long = 0): Array[Byte] =
    // folding : s._1 is the array, and s._2 is a counter
    msg.foldLeft(Array[Byte](),0){(s, c) => (s._1 :+ (c+key(s._2 % key.length)).toByte, s._2+1)}._1

  def _decrypt (msg: Array[Byte], key : Array[Byte]): Array[Byte] =
    _encrypt (msg, key)
}

class EncapsulatedVigenere extends EncapsulatedCrypto
{
  type T = Array[Byte]
  val crypto = new Vigenere()
  def makeKey(seed: Long) = (new VigenereKey(seed), "")
  def encrypt(msg: String, key: T, seed: Long) = crypto.encrypt(msg, key, seed)
  def decrypt(msg: String, key: T) = crypto.decrypt(msg, key)
}

class VigenereOpponent
{
  def indexOfCoincidence (inpt: String) = 
    ((inpt groupBy (c => c) mapValues (_.length.toFloat)).values.foldLeft (0.)
      {(s,c)=> s*(s-1)+c}) / (inpt.length*(inpt.length-1)/26)

  def filterString(str: String) = 
    str.filter {x=> (x > 30  && x < 126) || x == 32 }

  def getKeyLength (crypt: String) = 
  {
    def computeMoyIndex (s: String, k: Int) = 
    {
      def splitAt(x: Int) = 
      {
        var tmp = ""
        for (i<- 0 to s.length-1 if i%k == x) tmp += s(i)
        tmp
      }
      ( ((0 to k-1) map (x => indexOfCoincidence 
                            (splitAt (x)))).foldLeft (0.) {(s,c) => s+c} ) / k
    }
    ((0 to crypt.length/2) map (x => (computeMoyIndex(crypt, x), x)) min)._2
  }
}

