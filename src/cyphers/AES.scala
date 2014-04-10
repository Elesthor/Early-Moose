////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//              [AES.scala]                                                   //
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

import scala.BigInt
import scala.math


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            Precomputed Tables                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

object AES_TABLES
{
  val RCON= Array(
    0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8,
    0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3,
    0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f,
    0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d,
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab,
    0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d,
    0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25,
    0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01,
    0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d,
    0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa,
    0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a,
    0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02,
    0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a,
    0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef,
    0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94,
    0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04,
    0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f,
    0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5,
    0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33,
    0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb).map(_.toByte)

  val SBOX = Array(
    0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE,
    0xD7, 0xAB, 0x76, 0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 
    0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0, 0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 
    0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15, 0x04, 0xC7, 0x23, 0xC3, 
    0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75, 0x09, 
    0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 
    0x2F, 0x84, 0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 
    0x39, 0x4A, 0x4C, 0x58, 0xCF, 0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 
    0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8, 0x51, 0xA3, 0x40, 0x8F, 0x92, 
    0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2, 0xCD, 0x0C, 
    0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 
    0x73, 0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 
    0xDE, 0x5E, 0x0B, 0xDB, 0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 
    0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79, 0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 
    0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08, 0xBA, 0x78, 0x25, 
    0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A, 
    0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 
    0xC1, 0x1D, 0x9E, 0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 
    0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF, 0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 
    0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16).map(_.toByte)

  val INVERT_SBOX = Array(
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81,
    0xf3, 0xd7, 0xfb, 0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e,
    0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb, 0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 
    0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e, 0x08, 0x2e, 0xa1, 0x66, 
    0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25, 0x72, 
    0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 
    0xb6, 0x92, 0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 
    0x57, 0xa7, 0x8d, 0x9d, 0x84, 0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 
    0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06, 0xd0, 0x2c, 0x1e, 0x8f, 0xca, 
    0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b, 0x3a, 0x91, 
    0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 
    0x73, 0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 
    0x1c, 0x75, 0xdf, 0x6e, 0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 
    0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b, 0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 
    0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4, 0x1f, 0xdd, 0xa8, 
    0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f, 
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 
    0xc9, 0x9c, 0xef, 0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 
    0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61, 0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 
    0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d).map(_.toByte)


  def getRconValue(index: Byte)       = RCON(index & 0xFF)
  def getSBoxValue(index: Byte)       = SBOX(index & 0xFF)
  def getInvertSboxValue(index: Byte) = INVERT_SBOX(index & 0xFF)
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                              Key Generator                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class AESKey(seed: Int) extends Key [List[BigInt]]
{

////////////////////////////////////////////////////////////////////////////////
//                            Utilities functions                             //
////////////////////////////////////////////////////////////////////////////////

  val SIZE_16 = 16
  val SIZE_24 = 24
  val SIZE_32 = 32

// Rotate the 32-bit word 8 bits to the left 
  def rotate(word32: BigInt) = 
  {
    val inputBytes = (word32).toByteArray
    val r = inputBytes(0)
    for (i<- 0 to 2) inputBytes(i) = inputBytes(i+1)
    inputBytes(3) = r
    BigInt(inputBytes)
  }

// Main transformation per turn of the key expansion
  def core(wordBytes: Array[Byte], iteration: Int) = 
  {
    // Apply S-Box substitution on all 4 parts of the 32-bit word 
    for (i <- 0 to 3) wordBytes(i) = AES_TABLES.getSBoxValue(wordBytes(i))
    //  XOR the output of the rcon operation with i to the first part only
    wordBytes(0) = (wordBytes(0)^AES_TABLES.getRconValue(iteration.toByte)).toByte
    BigInt(wordBytes)
  }

////////////////////////////////////////////////////////////////////////////////
//                               Key Expansion                                //
////////////////////////////////////////////////////////////////////////////////

  def expandKey(initialKey: BigInt,  keySize: Int, expandedKeySize: Int) = 
  {
    // Current expanded keySize, in bytes */
    val key = initialKey.toByteArray
    var expandedKey = Array.fill(expandedKeySize)(0.toByte)
    var currentSize = 0
    var rconIteration = 1
    var tmp = Array(0.toByte, 0.toByte, 0.toByte, 0.toByte)  // temporary 4-byte variable
    //  set the 16,24,32 bytes of the expanded key to the input key 
    for (i <- 0 to keySize-1 ) expandedKey(i) = key(i)
    currentSize += keySize;
    while (currentSize < expandedKeySize)
    { 
      // Assign the previous 4 bytes to the temporary value t 
      for (i <-  0 to 3) tmp(i) = expandedKey((currentSize - 4) + i)
    
      // Every 16,24,32 bytes we apply the core schedule to t and increment 
      // rconIteration afterwards 
      if(currentSize % keySize == 0)  core(tmp, rconIteration)
      rconIteration += 1
      // For 256-bit keys, we add an extra sbox to the calculation
      if(keySize == SIZE_32 && ((currentSize % keySize) == 16)) 
      {
        for(i<- 0 to 3) tmp(i) = AES_TABLES.getSBoxValue(tmp(i));
      }
      for(i <- 0 to 3)  
      {
        expandedKey(currentSize) = (expandedKey(currentSize - keySize) ^ tmp(i)).toByte
        currentSize += 1
      }
    }
    var result = List(BigInt(0))
    val currentSubKey = Array.fill(16)(0.toByte)
    for (i<- 0 to expandedKey.length-1)
    {
      currentSubKey(i%16)  = expandedKey(i)
      if (i%16==15) result = BigInt(currentSubKey)::result
    } 
  result.reverse.drop(1)
  }

  def generate() = 
  {
    val randomizer = new util.Random(seed)
    val initialKey = BigInt(0x80, randomizer)
    expandKey(initialKey, 16 ,172)
  }
  val keys = generate()
  def getPublic() = keys
  def getPrivate() = keys
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                              AES State                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class AESState(input: BigInt)
{
  def init(inputBlock: BigInt) = 
  {
    var inputBytes = (inputBlock).toByteArray
    Array.tabulate[Byte](4,4) { (i,j) => inputBytes(i+4*j) }
  }

  def out() = 
  {
    val outputArray = Array.fill(0x10)(0.toByte)
    for (i<- 0 to 3; j <- 0 to 3) outputArray(i+4*j) = currentState(i)(j)
    BigInt(outputArray)
  }

  def printState()
  {
    for (i<-0 to 3) printf("%h, %h, %h, %h \n", currentState(i)(0), currentState(i)(1),
                                                currentState(i)(2), currentState(i)(3))
  }

  def subBytes(indirect: Boolean) = 
  {    
    // Select the correct s-box, either (indirect) or not.
      val sbox = (if (indirect) AES_TABLES.getInvertSboxValue(_) else AES_TABLES.getSBoxValue(_))
      for (i <- 0 to 3; j <-0 to 3) currentState(i)(j) = sbox(currentState(i)(j))
  }

  def shiftRows(indirect: Boolean) =
  {
    var t = Array(0.toByte,0.toByte,0.toByte, 0.toByte)
    for (i <- 0 to 3)
    {
      for (j <- 0 to 3) 
      {
        t((if (indirect) (j + i) % 4 else j)) =  
          (currentState(i))(if (indirect) j else (j + i) % 4)
      }
      for (j <- 0 to  3) currentState(i)(j) = t(j);
     }
  }

  // Multiplication function over GF(2^8) used in the mixColumns pass
  def GFMult(q: Byte, l: Byte ) =  
  {
    var r = 0.toByte 
    var t = 0.toByte
    var a = q
    var b = l
    while (a != 0) 
    {
      if ((a & 1) != 0) r = (r ^ b).toByte
      t =  (b & 0x80).toByte; b =  (b << 1).toByte
      if (t != 0) b =  (b ^ 0x1b).toByte
      a =  ((a & 0xff) >> 1).toByte
    }
    r.toByte
  }

  // AES mixColums and InvmixColumns passes
  def mixColumns(indirect: Boolean) = 
  {
    val tmp = Array(0.toByte,0.toByte, 0.toByte,0.toByte)
     val a =  (if (indirect)  0x0b else 0x03).toByte
     val b =  (if (indirect)  0x0d else 0x01).toByte
     val c =  (if (indirect)  0x09 else 0x01).toByte
     val d =  (if (indirect)  0x0e else 0x02).toByte
    for (i <- 0 to 3) 
    {
      tmp(0) = (GFMult(d, currentState(0)(i)) ^ GFMult(a, currentState(1)(i)) ^ 
                GFMult(b, currentState(2)(i)) ^ GFMult(c, currentState(3)(i))).toByte
      tmp(1) = (GFMult(c, currentState(0)(i)) ^ GFMult(d, currentState(1)(i)) ^ 
                GFMult(a, currentState(2)(i)) ^ GFMult(b, currentState(3)(i))).toByte
      tmp(2) = (GFMult(b, currentState(0)(i)) ^ GFMult(c, currentState(1)(i)) ^ 
                GFMult(d, currentState(2)(i)) ^ GFMult(a, currentState(3)(i))).toByte
      tmp(3) = (GFMult(a, currentState(0)(i)) ^ GFMult(b, currentState(1)(i)) ^ 
                GFMult(c, currentState(2)(i)) ^ GFMult(d, currentState(3)(i))).toByte
      for ( j <-  0 to 3 ) currentState(j)(i) =  (tmp(j));  
    }
  }

  def addRoundKey(roundKey: BigInt) = 
  {
    val inputBytes = (roundKey).toByteArray
    val tmp = Array.tabulate[Byte](4,4) { (i,j) => (inputBytes(i+4*j))}
    for (i<-0 to 3; j<-0 to 3) currentState(i)(j) = (currentState(i)(j)^tmp(i)(j)) .toByte
  }

  var currentState = init(input)
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                        AES Encryption, one block                           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class AESOneBlock
{
  def oneTurnDirect (state: AESState, roundKey: BigInt) =
  {
    state.subBytes(false)
    state.shiftRows(false)
    state.mixColumns(false)
    state.addRoundKey(roundKey)
  }

  def oneTurnInDirect(state: AESState, roundKey: BigInt) = 
  {
    state.addRoundKey(roundKey)
    state.mixColumns(true)
    state.shiftRows(true)
    state.subBytes(true)
  }

  def encrypt(state: AESState, keys: List[BigInt]) = 
  {
    val lastKey   = keys(keys.length-1)
    val roundKeys = keys.dropRight(1)
    state.addRoundKey(roundKeys(0))
    for (i <- roundKeys.drop(1)) oneTurnDirect(state, i)
    state.subBytes(false)
    state.shiftRows(false)
    state.addRoundKey(lastKey)
    state
  }

  def decrypt(state: AESState, keys: List[BigInt]) = 
  {
    val lastKey = keys(0) 
    val reversedKeys = keys.reverse.dropRight(1)
    state.addRoundKey(reversedKeys(0))
    state.shiftRows(true)
    state.subBytes(true)
    for (i <- reversedKeys.drop(1))  oneTurnInDirect(state, i)
    state.addRoundKey(lastKey)
    state
  }
}


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                        AES Encryption, CBC mode                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


class AES extends CryptoSystem [List[BigInt]]
{

////////////////////////////////////////////////////////////////////////////////
//                            Utilities functions                             //
////////////////////////////////////////////////////////////////////////////////

  def splitInBlocks(input: String) = 
  {
    val inputBytes = input.getBytes
    val paddedInput =  inputBytes ++ List.fill(0x10-(inputBytes.length%0x10))(0.toByte)
    val currentBlock = Array.fill(0x10)(0.toByte)
    var result = List(BigInt(0))
    for (i<- 0 to paddedInput.length-1)
    {
      currentBlock(i%0x10)  = paddedInput(i)
      if (i%0x10==0xf) result = BigInt(currentBlock)::result
    } 
  result.reverse.drop(1).map{x=> new AESState(x)}
  }

  def reconstructString(blocks: List[BigInt]) = 
  {
    var concatenatedBytes = blocks.foldLeft(Array[Byte]()){(s, c) => s++c.toByteArray}
    // Remove padding
    var currentPos = concatenatedBytes.length - 1
    var toDrop = 0
    while (currentPos > 0)
    {
      if (concatenatedBytes(currentPos) == 0.toByte)
      {
        toDrop += 1
        currentPos -= 1
      }
      else currentPos = 0
    }
    new String(concatenatedBytes.dropRight(toDrop))
  }

  def xorStates (state1: AESState, state2: AESState ) = 
  {
    val result = (new AESState (BigInt(0x80, new util.Random(423))))
    val content = Array.tabulate[Byte](4,4) 
    { (i,j) => (state1.currentState(i)(j)^state2.currentState(i)(j)).toByte}
    result.currentState = content
    result
  }

////////////////////////////////////////////////////////////////////////////////
//                          Encryption functions                              //
////////////////////////////////////////////////////////////////////////////////

  def CBCModeEncrypt(keys: List[BigInt], blocks: List[AESState]) = 
  {
    val cypher = new AESOneBlock()
    var IV = (new AESState (BigInt(0x80, new util.Random(43))))
    var result = List[AESState]()
    for (i<- blocks)
    {
      IV = (cypher.encrypt(xorStates(IV, i), keys))  
      result = IV::result
    }
    (result.map(_.out)).reverse
  }

  def CBCModeDecrypt(keys: List[BigInt], blocks: List[AESState]) = 
  {
    val cypher = new AESOneBlock()
    var IV = (new AESState (BigInt(0x80, new util.Random(43))))
    var result = List[BigInt]()
    for (i<- blocks)
    {
      val t = new AESState (BigInt(0x80, new util.Random(43)))
      val content = Array.tabulate[Byte](4,4) 
                      { (x,y) => i.currentState(x)(y).toByte}
      t.currentState = content

      result = (xorStates(cypher.decrypt(t, keys), IV)  ).out::result 
      IV = i
    }
    result.reverse
  }
  
////////////////////////////////////////////////////////////////////////////////
//                              Decryption functions                          //
////////////////////////////////////////////////////////////////////////////////

  def _encrypt (msg: Array[Byte], key: List[BigInt], seed: Int = 0) = 
  {
    CBCModeEncrypt(key, splitInBlocks(new String(msg))).foldLeft("")
                                                      {(s,c)=>s+c+","}.getBytes
  }

  def _decrypt (msg: Array[Byte], key: List[BigInt]) = 
  {
    reconstructString(CBCModeDecrypt(key, new String (msg).split(""",""").map
                              {x => new AESState(BigInt(x))}.toList)).getBytes
  }

}


