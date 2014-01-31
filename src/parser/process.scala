abstract class Process

class Channel

// 0
case class PTrivial extends Process

// in(c, x).p
case class PIn(c:Channel, x:TVar, p:Process) extends Process

// in^k(c, x->u as y).p
case class PInk(c:Channel, x:TVar, u:Term, y:TVar, p:Process) extends Process

// out(c, t).p
case class POut(c:Channel, t:Term, p:Process) extends Process

// if v then pIf else pElse
case class PIf(v:TVal, pIf:Process, pElse:Process) extends Process

// new s.p
case class PNew(s:TVal, p:Process) extends Process
