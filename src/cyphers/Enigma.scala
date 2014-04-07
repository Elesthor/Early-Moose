////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//              [Enigma.scala]                                                //
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
//                                Enigma Key                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


class EnigmaKey (seed: Int) extends Key [(List[Rotor],Rotor)]
{
  // Possible Rotors (I,II,II,IV,V,beta,gamma)
  val ROTORS = List (
    "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    "BDFHJLCPRTXVZNYEIWGAKMUSQO",
    "VZBRGITYUPSDNHLXAWMJQOFECK",
    "JPGVOUMFYQBENHZRDKASXLICTW",
    "NZJHGRCXMYSWBOUFAIVLPEKQDT",
    "FKQHTLXOCBJSPDZRAMEWNIUYGV",
    "LEYJVCNIXWPBQMDRTAKZGFUHOS",
    "FSOKANUERHMBTIYCWLQPZXVGJD"
  )
  // Possible reflectors
  val REFLECTORS = List ( 
    "EJMZALYXVBWFCRQUONTSPIKHGD",
    "YRUHQSLDPXNGOKMIEBFZCWVJAT",
    "FVPJIAOYEDRZXWGCTKUQSBNMHL"
  )

  // Randomly select four rotors, one reflector and set the initial position.
  def generate(): (List[Rotor],Rotor) = 
  {
    val factory         = new RotorFactory
    val randomizer      = new util.Random(seed) 
    def randBuff()      = randomizer.nextInt(26) // Will create the random 
                                                 // initail offset
    val baseRotors      = ROTORS
    val baseReflectors  = REFLECTORS(randomizer.nextInt(3))
    val rotors =  randomizer.shuffle(baseRotors).drop(5).map
                  {s=>factory.getRotor(s, randBuff())} 
    val reflector = factory.getRotor(baseReflectors, 0) 
    return (rotors, reflector)   
  }

  val (rotors, reflector) = generate()
  def getPublic = (rotors, reflector)
  def getPrivate = getPublic
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             Rotors definition                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

trait Rotor
{
  var buffer:             Int       // Current position of the rotor
  val initialBuff:        Int       // Initial position of the rotor
  val content:            String    // Table of permutation, given as a string
  val invertedContent:    String    // Inverse of the previous table
  def rotate():           Unit      // Turn the rotor of 1/26 turn 
  def target(i: Char):    Char      // Give the image of i by the permutation
  def targetRev(i: Char): Char      // Give the image of i in the way-back
}


////////////////////////////////////////////////////////////////////////////////
//                            Creation of rotors                              //
////////////////////////////////////////////////////////////////////////////////

class RotorFactory
{
  // Give the invert permutation of a content.
  def invert(content: String): String = 
  {
    val result = Array.fill(26)(0.toChar)
    for (i<-0 to 25)
    {
      result(content(i)-65) = (65+i).toChar 
    }
    result.foldLeft(""){(x,s)=>x+s}
    result.foldLeft(""){(x,s)=>x+s}
  }
  
  // Factory's builder
  def getRotor(cont: String, initialBuffer: Int): Rotor = 
  {
    object newRotor extends Rotor 
    {
      val content         = cont
      var buffer          = initialBuffer
      val initialBuff     = initialBuffer
      val invertedContent = invert(cont)

      def rotate() = buffer = (buffer+1)%26

      // Get the image of char i by the rotor (direct path) 
      def target(i: Char) = 
      {
        var pos = (i-65+buffer)%26
        content(pos)
      }

      // Get the image of char i by the rotor (reverse path)
      def targetRev(i: Char) = 
      {
        var pos = (i-65)%26
        ((invertedContent(pos)-39-buffer)%26+65).toChar
      }
    }
    return newRotor
  }
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Enigma implementation                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


// Implementation of a Enigma 1941 model for KriegsMarine  ( 4 Rotors )  
class Enigma extends CryptoSystem [(List[Rotor],Rotor)]
{
  // Count the number of letters crypted to make rotors 1,2,3 turn.
  var state = Array(0, 0, 0)

  // Put the rotors in the initial position
  def replaceInInitialState(rotors: List[Rotor]) = 
  {
    state = Array(0,0,0)
    rotors.map({x => x.buffer = x.initialBuff} )
  }

  // Crypt one letter, and append the modification to the rotors's positions
  def oneTurn (i: Char, rotors: List[Rotor], reflector: Rotor)=
  {
    if (64 < i && i < 91) // Only crypt capital letters.
    {
      state(0) = (state(0)+1)%26
      state(1) = (state(1)+1)%(26*26)
      state(2) = (state(2)+1)%(26*26*26)

      val firstPass = rotors(3).target(rotors(2).target(
                      rotors(1).target(rotors(0).target(i))))
      val reflected = reflector.target(firstPass)
      val result    = rotors(0).targetRev(rotors(1).targetRev(
                      rotors(2).targetRev(rotors(3).targetRev(reflected))))
      
      rotors(0).rotate() // First rotor turns at each letter
      if (state(0) == 0) rotors(1).rotate() // first rotor had completly turned
      if (state(1) == 0) rotors(2).rotate() // snd rotor had completly turned 
      if (state(2) == 0) rotors(3).rotate() // and the third too
      result  
    }
    else i
  }

  def _encrypt(msg: Array[Byte], key: (List[Rotor], Rotor), _seed: Int)  = 
  {
    val ( rotors, reflector ) = key
    replaceInInitialState(rotors)
    (new String (msg)).map({x => oneTurn(x, rotors, reflector)}).getBytes
  }

  def _decrypt(msg: Array[Byte], key:(List[Rotor],Rotor)) = 
  {
    _encrypt(msg, key, 0)
  }
}
