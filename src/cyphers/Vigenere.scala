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

class VigenereKey(seed: Int) extends Key[Array[Byte]]
{
  def generate() : Array[Byte] =
  {
    // random Array[Byte] of random size
    val randomizer = new util.Random(seed)
    // nextString return string of any character (chinese...), we cast them to bytes
    randomizer.nextString((randomizer.nextInt() % 8) +6).toCharArray.map(_.toByte)
  }
  
  def getPublic  = generate()
  def getPrivate = getPublic.map({x => (-x).toByte})
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            Vigenere Implementation                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Vigenere extends CryptoSystem [Array[Byte]]
{
  def _encrypt (msg: Array[Byte], key: Array[Byte], _seed:Int = 0): Array[Byte] =
    // folding : s._1 is the array, and s._2 is a counter
    msg.foldLeft(Array[Byte](),0){(s, c) => (s._1 :+ (c+key(s._2 % key.length)).toByte, s._2+1)}._1

  def _decrypt (msg: Array[Byte], key : Array[Byte]): Array[Byte] =
    _encrypt (msg, key)
}



class EncapsulatedVigenere extends EncapsulatedCrypto
{
  type T = Array[Byte]
  val crypto = new Vigenere()
  def makeKey(seed: Int) = new VigenereKey(seed)
  def encrypt(msg: String, key: Key[T]) = crypto.encrypt(msg, key)
  def decrypt(msg: String, key: Key[T]) = crypto.decrypt(msg, key)
}


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            Vigenere Opponent                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class VigenereOpponent
{

////////////////////////////////////////////////////////////////////////////////
//                      String manipulation Toolbox                           //
////////////////////////////////////////////////////////////////////////////////


  // Compute the index of coincidence of the inpt string.
  def indexOfCoincidence (inpt: String) = 
    ((inpt groupBy (c => c) mapValues (_.length.toFloat)).values.foldLeft (0.)
      {(s,c)=> c*(c-1)+s}) / (inpt.length*(inpt.length-1))

  
  // Remove all non printable characters from the str string.
  def filterString(str: String) = 
    str.filter {x=> (x > 47  && x < 92) || (96 < x && x < 123) || List(32, 40, 41, 44, 46, 58, 93 ).contains(x) }

  // Compute the substring constituted from the chars at positions which are 
  // congrous to x mod k.
  def splitAt(s: String, x: Int, k: Int) = 
  {
    var tmp = ""
    for (i<- 0 to s.length-1 if i%k == x) tmp += s(i)
    tmp
  }


  // Merge a list of list (generalisation of the zip function)
  def combineLists[A](ss:List[A]*) = 
    (ss.head.map(List(_)) /: ss.tail)(_.zip(_).map(p=>p._2 :: p._1))


  // Zip a list of strings 
  def recombine(l: List[String]) =
  {
    val maxLength = l.map(_.length).max;
    val completedList = l.map(s => s+" "*(maxLength-s.length))
    combineLists((completedList.map ( s => s.reverse.toList )):_*).map (_.mkString).foldLeft ("")((s,c)=>s+c).reverse
  }

////////////////////////////////////////////////////////////////////////////////
//                        Cryptanalysis functions                             //
////////////////////////////////////////////////////////////////////////////////
  

  // Compute the most probable key length, according to a given index of 
  // coincidence for the language.
  def getKeyLength (crypt: String) = 
  {
    def computeMoyIndex (s: String, k: Int) = 
    {
    (((0 to k-1) map (x => indexOfCoincidence 
      (splitAt (s, x, k)))).foldLeft (0.) {(s,c) => s+c} ) / k
    }
    (((1 to 3) map 
      (x => (scala.math.abs(computeMoyIndex(crypt, x)-0.0662), x))) min)._2
  }
  

  // Return the closest cesar decryption for a given string, for the 
  // euclidian distance of the frequencies table for the language.
  def getClosestDec(crypt: String) =
  {
    val cypher = new Cesar()
    val currentKey = new CesarKey(0) 
    val opp = new CesarOpponent();
    (0 to 255).map (i => {currentKey.setKey(i);
    cypher.decrypt(crypt, currentKey)}).filter(x => x==filterString(x)).map(x=>(opp.computeDist(opp.getLetters(x)), x)).min._2
      //.sortWith((x,y) => x._1 <= y._1)
  }

  def openEnc (crypt: String, k: Int) = 
  {
    //val keyLength = getKeyLength(crypt);
    val keyLength = k;
    val splited = ((0 to keyLength-1) map 
        (x =>  (splitAt (crypt, x, keyLength))))
    recombine(splited.map(x=>getClosestDec(x)).toList)

  }
}
val c = new CesarOpponent()
val cypher = new Vigenere();
val opp = new VigenereOpponent();
val toto = "[(1234,111098765432467890pai897654pair37689987656438903, pair)]"
val key = new VigenereKey(5);
println(key.getPublic.length)
val crypt = cypher.encrypt(toto, key)
println(crypt);
println(opp.indexOfCoincidence(toto))
println(opp.getKeyLength(crypt))
for (i<- 1 to 13)
{
  try
  {
  val sp = opp.openEnc(crypt,i)
  println(i+sp+"   "+ c.computeDist(sp), c.computeDist(toto))
  
  }
  catch
  {
    case e: Exception => println("coucou")
  }
}

