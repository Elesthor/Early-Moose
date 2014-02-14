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
  def RetString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
}

class MetaProc(pLeft: Process, k: Int, metaPRight: Option[MetaProc]){
  def RetString(x: Int): String = 
  {
    "| "*x+"MetaProc: (^"+k+")\n"+pLeft.RetString(x+1)+
    (
      metaPRight match
      {
        case None         => ""
        case Some(pRight) => pRight.RetString(x) // pas +1 pour ne pas faire apparaitre à la meme hauteur
      }
    )
  }
}

abstract class Process{
  def RetString (x: Int): String
  
  /*def Next() : Option[Process] =
  {
    this match
    {
      case PTrivial()             => None
      case PIn(_, _, p)           => Some(p)
      case PInk(_, _, _, _, _, p) => Some(p)
      case POut(_, _, p)          => Some(p)
      case PIf(_, _, _, p)        => Some(p)
      case PNew(_, p)             => Some(p)
      case PSeq(_, p)             => Some(p)
    }
  }
  
  def Last() : Option[Process] =
  {
    Next() match
    {
      case None    => None // PTrivial
      case Some(p) =>
        p.Next() match
        {
          case None     => Some(this) // next is PTrivial
          case Some(p2) => p.Next()
        }
    }
  }*/
}

case class PTrivial() extends Process{
  def RetString(x: Int) = "| "*x+"•\n"
    //def RetString (x: Int): String = "Trivial Process: 0"
}
case class PIn (c: Channel, v: TVar, p: Process) extends Process{
  def RetString(x: Int) = "| "*x+"PIn:\n"+c.RetString(x+1)+v.RetString(x+1)+p.RetString(x)
}
case class PInk(c: Channel, v: TVar, u: Term, y: TVar, k: Int, p: Process) extends Process{
  def RetString(x: Int) = "| "*x+"PInk:\n"+c.RetString(x+1)+v.RetString(x+1)+u.RetString(x+1)+y.RetString(x+1)+"| "*x+k+"\n"+p.RetString(x)
}
case class POut(c: Channel, t: Term, p: Process) extends Process{
  def RetString(x: Int) = "| "*x+"POut:\n"+c.RetString(x+1)+t.RetString(x+1)+p.RetString(x)
}
case class PIf (v: Value, pIf: Process, pElse: Process, p: Process) extends Process{
  def RetString(x: Int) = "| "*x+"PIf:\n"+v.RetString(x+1)+pIf.RetString(x+1)+pElse.RetString(x+1)
}
case class PNew(s: VConst, p: Process) extends Process{
  def RetString(x: Int) = "| "*x+"PNew:\n"+s.RetString(x+1)+p.RetString(x)
}
case class PSeq(l: Process, p: Process) extends Process{
  def RetString(x: Int) = "| "*x+"PSeq:\n"+l.RetString(x+1)+p.RetString(x+1)
}

