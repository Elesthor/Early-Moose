////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [GenericCypher.scala]                                            //
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

trait GenericCypher [T]
{
  def encrypt (msg: String , pub: Key [T] ): String
  def decrypt (msg: String , priv: Key [T] ): String
}

