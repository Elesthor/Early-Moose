////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//             [Cesar.scala]                                                  //
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
//                                Cesar Key                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class CesarKey (seed: Long) extends Key [Byte]
{
  var key = new util.Random(seed).nextInt.toByte
  def setKey (x: Int) = key = x.toByte
  def getPublic () = key
  def getPrivate = (-getPublic).toByte
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Cesar implementation                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Cesar extends CryptoSystem [Byte]
{
  def _encrypt (msg: Array[Byte], key: Byte, _seed:Long = 0): Array[Byte] =
    msg.map({ x => (x + key).toByte})

  def _decrypt (msg: Array[Byte], key: Byte): Array[Byte] =
    _encrypt (msg, key)
}

class EncapsulatedCesar extends EncapsulatedCrypto
{
  type T = Byte
  val crypto = new Cesar()
  def makeKey(seed: Long) = new CesarKey(seed)
  def encrypt(msg: String, key: Key[T]) = crypto.encrypt(msg, key)
  def decrypt(msg: String, key: Key[T]) = crypto.decrypt(msg, key)
}


class CesarOpponent 
{
  val FREQ_FR = scala.collection.immutable.Map( 
      ('A',9.2),('B',1.02),('C',2.64),('D',3.39),('E',15.87),('F',0.95),('G',1.04),
      ('H',0.77),('I',8.41), ('J',0.89),('K',0.00),('L',5.34),('M',3.24),('N',7.15),
      ('O',5.14),('P',2.86), ('Q',1.06),('R',6.46),('S',7.90),('T',7.26),('U',6.24),
      ('V',2.15),('W',0.00), ('X',0.30),('Y',0.24),('Z',0.32))

  
  // Compute the frequency table of the letters occurence in the string inpt.
  def letterFrequencies(inpt: String) =
    inpt groupBy (c => c) mapValues (_.length.toFloat/inpt.length*100)

  // Filter the string str from all non-printable ascii characters.
  def filterString(str: String) = 
    str.filter {x=> (x > 30  && x < 126) || x == 32 }

  def getLetters(str: String) = 
  {
   ( (str.toUpperCase).filter { x => (x>64 && x<91) }  )
  }
  
  
  def computeDist(str: String) = 
  {
    var sum = 0.;
    var baseFreq = letterFrequencies(str);
    for (i <- 65 to 90)
    {
      val bqp = 
      (
        if (str.contains(i.toChar)) baseFreq(i.toChar)
        else 0.
      )
      sum +=  (bqp - FREQ_FR(i.toChar))*(bqp - FREQ_FR(i.toChar))
    }
    sum
  }


  def openEnc(crypt: String) =
  {
    val cypher = new Cesar()
    val currentKey = new CesarKey(0) 
    var minimalValue = 10000.
    var decryptedText = ""
    for (i<- 0 to 255)
    {
      currentKey.setKey(i);
      val currentDecrypt = cypher.decrypt(crypt, currentKey);
      if (currentDecrypt == filterString(currentDecrypt))
      {
        val currentDist =  computeDist(getLetters(currentDecrypt))
        if (minimalValue > currentDist)
        {
          minimalValue = currentDist;
          decryptedText = currentDecrypt
        }
      }
    }
    decryptedText
  }      
}

