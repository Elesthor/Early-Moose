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

//class EnigmaKey (seed: Int) extends Key [()]
//{
//  def generate() = 
//  {


//  }

  //def getPublic = 
  //def getPrivate = 

//}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             Rotors definition                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

abstract class Rotor
{
  val content: String
  var buffer: Int
  def rotate() = buffer = (buffer+1)%26
  def target(i: Char) = 
  {
    var pos = i-65+buffer
    content(pos)
  }
}

class RotorI(initialBuffer: Int) extends Rotor
{
  var buffer = initialBuffer
  val content = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
}

class RotorII(initialBuffer: Int) extends Rotor
{
  var buffer = initialBuffer
  val content = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
}

class RotorIII(initialBuffer: Int) extends Rotor
{
  var buffer = initialBuffer
  val content = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
}

class RotorIV(initialBuffer: Int) extends Rotor
{
  var buffer = initialBuffer
  val content = "ESOVPZJAYQUIRHXLNFTGKDCMWB"
}

class RotorV(initialBuffer: Int) extends Rotor
{
  var buffer = initialBuffer
  val content = "VZBRGITYUPSDNHLXAWMJQOFECK"
}

class EKW extends Rotor 
{
  val content = "ABCDEFGDIJKGMKMIEBFTCVVJAT"
  val buffer = 0
}


class Enigma extends CryptoSystem [Int]
{

  val state = Array(0,0)
  def oneTurn (i: Char)=
  {
    state(0) = (state(0)+1)%26
    state(1) = (state(1)+1)%(26*26)
    val firstPass = Rotor3.target(Rotor2.target(Rotor1.target(i)))
    val reflected = Reflector.target(firstPass)
    val result = Rotor1.target(Rotor2.target(Rotor3.target(reflected)))
    Rotor1.rotate()
    if (state(0) == 0) Rotor2.rotate()
    if (state(1) == 0) Rotor3.rotate()
    result
  }

  val Rotor1 = new RotorI(0)
  val Rotor2 = new RotorV(0)
  val Rotor3 = new RotorI(0)
  val Reflector = new EKW()
  println(oneTurn("A"(0)))
  println((oneTurn("A"(0))))
}

val a = new Enigma
