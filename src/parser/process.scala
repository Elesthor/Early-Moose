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



class MetaProc(pLefta: Process, ka: Int, metaPRighta: Option[MetaProc])
{
  val pLeft = pLefta
  val k = ka
  val metaPRight = metaPRighta

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

abstract class Process
{
  def RetString (x: Int): String
  def Replace(x: String , T: Term): Process
}

case class PTrivial() extends Process
{
  def RetString(x: Int) = "| "*x+"•\n"
    //def RetString (x: Int): String = "Trivial Process: 0"
  def Replace(x: String , T: Term): Process = new PTrivial()
}
case class PIn (c: Channel, v: TVar, p: Process) extends Process
{
  def RetString(x: Int) = "| "*x+"PIn:\n"+c.RetString(x+1)+v.RetString(x+1)+p.RetString(x)
  def Replace(x: String , T: Term): Process =
  {
    new PIn(c, v, p.Replace(x,T))
  }
}
case class PInk(c: Channel, v: TVar, u: Term, y: TVar, k: Int, p: Process) extends Process
{
  def RetString(x: Int) = "| "*x+"PInk: "+k+"\n"+c.RetString(x+1)+v.RetString(x+1)+u.RetString(x+1)+y.RetString(x+1)+p.RetString(x)
  def Replace(x: String , T: Term): Process =
  {
    new PInk(c, v, u.Replace(x,T),y ,k, p.Replace(x,T))
  }
}
case class POut(c: Channel, t: Term, p: Process) extends Process
{
  def RetString(x: Int) = "| "*x+"POut:\n"+c.RetString(x+1)+t.RetString(x+1)+p.RetString(x)
  def Replace(x: String , T: Term): Process =
  {
    new POut(c, t.Replace(x,T), p.Replace(x,T))
  }
}
case class PIf (v: Value, pIf: Process, pElse: Process) extends Process
{
  def RetString(x: Int) = "| "*x+"PIf:\n"+v.RetString(x+1)+pIf.RetString(x+1)+pElse.RetString(x+1)
  def Replace(x: String , T: Term): Process =
  {
    new PIf(v, pIf.Replace(x,T), pElse.Replace(x,T))
  }
}
case class PNew(s: VConst, p: Process) extends Process
{
  def RetString(x: Int) = "| "*x+"PNew:\n"+s.RetString(x+1)+p.RetString(x)
  def Replace(x: String , T: Term): Process =
  {
    new PNew(s, p.Replace(x,T))
  }
}
case class PSeq(l: Process, p: Process) extends Process
{
  def RetString(x: Int) = "| "*x+"PSeq:\n"+l.RetString(x+1)+p.RetString(x+1)
  def Replace(x: String , T: Term): Process =
  {
    new PSeq(l.Replace(x,T), p.Replace(x,T))
  }
}

