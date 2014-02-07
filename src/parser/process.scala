////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [process.scala]                                                  //
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


class Channel(c: String) {
  def RetString(x: Int): String = " "*x+"Channel:"+c+"\n"
} // Temporary class to reprend outputs channels.

class MetaProc(pLeft: Process, k: Int, metaPRight: Option[MetaProc]){
  def RetString(x: Int): String = 
  {
    " "*x+"MetaProc: "+k+"\n"+pLeft.RetString(x+1)+
    (
      metaPRight match
      {
        case None         => " "*(x+1)+"None\n"
        case Some(pRight) => pRight.RetString(x+1)
      }
    )
  }
}

abstract class Process{
  def RetString (x: Int): String
}

case class PTrivial() extends Process{
  def RetString(x: Int): String = " "*x+"Trivial\n"
    //def RetString (x: Int): String = "Trivial Process: 0"
}
case class PIn (c: Channel, v: TVar, p: Process) extends Process{
  def RetString(x: Int): String = " "*x+"PIn:\n"+c.RetString(x+1)+v.RetString(x+1)+p.RetString(x+1)
}
case class PInk(c: Channel, v: TVar, u: Term, y: TVar, k: Int, p: Process) extends Process{
  def RetString(x: Int): String = " "*x+"PInk:\n"+c.RetString(x+1)+v.RetString(x+1)+u.RetString(x+1)+y.RetString(x+1)+" "*x+k+"\n"+p.RetString(x+1)
}
case class POut(c: Channel, t: Term, p: Process) extends Process{
  def RetString(x: Int): String = " "*x+"POut:\n"+c.RetString(x+1)+t.RetString(x+1)+p.RetString(x+1)
}
case class PIf (v: Term, pIf: Process, pElse: Process) extends Process{
  def RetString(x: Int): String = " "*x+"POut:\n"+v.RetString(x+1)+pIf.RetString(x+1)+pElse.RetString(x+1)
}
case class PNew(s: VConst, p: Process) extends Process{
  def RetString(x: Int): String = " "*x+"PNew:\n"+s.RetString(x+1)+p.RetString(x+1)
}

