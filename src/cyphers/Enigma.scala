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

abstract class Key [T]
{
  def getPublic(): T
  def getPrivate(): T
}

trait CryptoSystem [T]
{
  def encrypt (msg: String , pub: Key [T] ): String
  def decrypt (msg: String , priv: Key [T] ): String
}

class EnigmaKey (seed: Int) extends Key [(List[Rotor],Rotor)]
{
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

  val REFLECTORS = List ( 
    "EJMZALYXVBWFCRQUONTSPIKHGD",
    "YRUHQSLDPXNGOKMIEBFZCWVJAT",
    "FVPJIAOYEDRZXWGCTKUQSBNMHL"
  )

  def generate(): (List[Rotor],Rotor) = 
  {
    val factory         = new RotorFactory
    val randomizer      = new util.Random(seed) 
    def randBuff()      = randomizer.nextInt(26)
    
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
  var buffer:             Int
  val initialBuff:        Int
  val content:            String
  val invertedContent:    String
  def rotate():           Unit  
  def target(i: Char):    Char  
  def targetRev(i: Char): Char 
}

class RotorFactory
{

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

  def getRotor(cont: String, initialBuffer: Int): Rotor = 
  {
    object newRotor extends Rotor 
    {
      val content         = cont
      var buffer          = initialBuffer
      val initialBuff     = initialBuffer
      val invertedContent = invert(cont)

      def rotate() = buffer = (buffer+1)%26

      def target(i: Char) = 
      {
        var pos = (i-65+buffer)%26
        content(pos)
      }

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

class Enigma extends CryptoSystem [(List[Rotor],Rotor)]
{
  var state = Array(0,0)

  def replaceInInitialState(rotors: List[Rotor]) = 
  {
    state = Array(0,0)
    rotors.map({x => x.buffer = x.initialBuff} )
  }

  def oneTurn (i: Char, rotors: List[Rotor], reflector: Rotor)=
  {
    if (64 < i && i < 91)
    {
      state(0) = (state(0)+1)%26
      state(1) = (state(1)+1)%(26*26)
      val firstPass = rotors(3).target(rotors(2).target(
                      rotors(1).target(rotors(0).target(i))))
      val reflected = reflector.target(firstPass)
      val result    = rotors(0).targetRev(rotors(1).targetRev(
                      rotors(2).targetRev(rotors(3).targetRev(reflected))))
      rotors(0).rotate()
      if (state(0) == 0) rotors(1).rotate()
      if (state(1) == 0) rotors(2).rotate()
      result  
    }
    else i
  }

  def encrypt(msg: String, key: Key[(List[Rotor],Rotor)]): String = 
  {
    val ( rotors, reflector ) = key.getPublic
    replaceInInitialState(rotors)
    msg.map({x => oneTurn(x, rotors, reflector)})
  }

  def decrypt(msg: String, key: Key[(List[Rotor],Rotor)]): String = 
  {
    encrypt(msg, key)
  }
}

val a = new Enigma
val test ="!!!ABCDEFGHIJKLMNOPQRSTUVWXYZABCD"
val b = new EnigmaKey(42)
val x = a.encrypt(test, b)

println(x)
println(a.decrypt( x,b))

