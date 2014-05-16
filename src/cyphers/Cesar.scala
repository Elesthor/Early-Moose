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

class CesarKey (seed: Int) extends Key [Byte]
{
  def getPublic = seed.toByte//new util.Random(seed).nextInt.toByte TODO remettre
  def getPrivate = (-getPublic).toByte
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Cesar implementation                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class Cesar extends CryptoSystem [Byte]
{
  def _encrypt (msg: Array[Byte], key: Byte, _seed:Int = 0): Array[Byte] =
    msg.map({ x => (x + key).toByte})

  def _decrypt (msg: Array[Byte], key: Byte): Array[Byte] =
    _encrypt (msg, key)
}

class EncapsulatedCesar extends EncapsulatedCrypto
{
  type T = Byte
  val crypto = new Cesar()
  def makeKey(seed: Int) = new CesarKey(seed)
  def encrypt(msg: String, key: Key[T]) = crypto.encrypt(msg, key)
  def decrypt(msg: String, key: Key[T]) = crypto.decrypt(msg, key)
}

