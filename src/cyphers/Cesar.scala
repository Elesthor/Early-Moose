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
  def getString(k: Byte): String =
    k.toString
  def fromString(s: String): Byte =
    s.toByte
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
  def makeKey(seed: Long) = (new CesarKey(seed), "")
  def encrypt(msg: String, key: T, seed: Long) = crypto.encrypt(msg, key, seed)
  def decrypt(msg: String, key: T) = crypto.decrypt(msg, key)
}


class CesarOpponent extends Opponent
{
  val FREQ_FR = scala.collection.immutable.Map( 
      ('!',0.0427497311748),('%',0.000546763515274),(')',1.37707006828),
      ('(',1.37701272883), ('+',6.09756097561e-05), ('-',0.0699495741795),
      (',',1.67779290886), ('/',0.000569363676566),('.',0.369103254673),
      ('1',6.84596406525),('0',6.1201961431),('3',6.69875001885),
      ('2',6.62342654969),('5',6.76489186998),('4',6.64340751543),
      ('7',6.71723783619), ('6',6.50060647488),('9',6.5606388158),
      ('8',6.59759144067),(';',0.00999405387986),(':',3.01142446203),
      ('?',0.0649862964664),('A',2.92366108479),('C',0.370234077642),
      ('B',0.355911821756),('E',2.2405857491),('D',0.832105634308),
      ('G',0.475451798364), ('F',0.37482563323),('I',2.79232391368),
      ('H',1.21080206944),('K',0.147350957831),('J',0.0594822133613),
      ('M',0.442836438055), ('L',0.712119615578),('O',1.68523284479),
      ('N',1.16642172444),('Q',0.000551000752754),('P',1.62920836561),
      ('S',1.11592692364), ('R',2.25477591137),('U',0.528778101758),
      ('T',1.90436916684),('W',0.536821827016),('V',0.180590633963),
      ('Y',0.476629186144),('X', 0.0), ('Z', 0.0),('[',0.384171689195),(']',0.384132976566),
      ('<', 0.0), ('=', 0.0), ('>', 0.0), ('@', 0.0))

  // Compute the frequency table of the letters occurence in the string inpt.
  def letterFrequencies(inpt: String) =
    inpt groupBy (c => c) mapValues (_.length.toFloat/inpt.length*100)

  // Filter the string str from all non-printable ascii characters.
 def filterString(str: String) = 
    str.filter 
    {x=> (x > 47  && x < 59) || (x > 64 && x < 92) || (96 < x && x < 123) || List(32, 40, 41, 44, 46, 58, 93).contains(x) }

  def getLetters(str: String) = 
  {
   ( (str.toUpperCase).filter {x=> (x > 47  && x < 92) || List(40, 41, 44, 46, 58, 93 ).contains(x) }  )
  }
  
  
  def computeDist(str: String) = 
  {
    var sum = 0.0;
    var baseFreq = letterFrequencies(str);
    for (i <- (48 to 91).toList ++  List(40, 41, 44, 46, 58, 93 ))
    {
      val bqp = 
      (
        if (str.contains(i.toChar)) baseFreq(i.toChar)
        else 0.0
      )
      sum +=  (bqp - FREQ_FR(i.toChar))*(bqp - FREQ_FR(i.toChar))
    }


    sum
  }

  def openEnc(crypta: Array[Byte], _infos : Term):Array[Byte] =
  {
    val crypt = new String(crypta, java.nio.charset.Charset.forName("ISO-8859-1"))
    val cypher = new Cesar()
    val currentKey = new CesarKey(0) 
    var minimalValue = 10000.0
    var decryptedText = ""
    for (i<- 0 to 255)
    {
      currentKey.setKey(i);
      val currentDecrypt = cypher.decrypt(crypt, currentKey.getPublic);
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
    decryptedText.getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
  }      
}

