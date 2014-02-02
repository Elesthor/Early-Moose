////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [process.scala]                                                  //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//

class Channel // Temporary class to reprend outputs channels.

abstract class Process{
    //def retString (x: Int): String
}

case class PTrivial extends Process{
    //def retString (x: Int): String = "Trivial Process: 0"
}
case class PIn (c: Channel, x: TVar, p: Process)                 extends Process
case class PInk(c:Channel, x: TVar, u: Term, y: TVar, p: Process)extends Process
case class POut(c: Channel, t: Term, p: Process)                 extends Process
case class PIf (v: TValue, pIf: Process, pElse: Process)         extends Process
case class PNew(s: VConst, p: Process)                           extends Process

