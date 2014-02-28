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


import scala.collection.mutable.SynchronizedQueue

class Channel(c: String)
{
  val name: String = c
  def retString(x: Int): String = "| "*x+"Channel:\n"+"| "*(x+1)+c+"\n"
  var content: SynchronizedQueue[String] = new SynchronizedQueue()
}

class MetaProc(pLefta: Process, ka: Int, metaPRighta: Option[MetaProc])
{
  val pLeft = pLefta
  val k = ka
  val metaPRight = metaPRighta

  def retString(x: Int): String =
  {
    "| "*x+"MetaProc: (^"+k+")\n"+pLeft.retString(x+1)+
    (
      metaPRight match
      {
        case None         => ""
        case Some(pRight) => pRight.retString(x) // pas +1 pour ne pas faire apparaitre à la meme hauteur
      }
    )
  }
}

abstract class Process
{
  def retString (x: Int): String
  def replace(x: String , T: Term): Process
}

case class PTrivial() extends Process
{
  def retString(x: Int) = "| "*x+"•\n"
    //def retString (x: Int): String = "Trivial Process: 0"
  def replace(x: String , T: Term): Process = new PTrivial()
}
case class PIn (c: Channel, v: TVar, p: Process) extends Process
{
  def retString(x: Int) = "| "*x+"PIn:\n"+c.retString(x+1)+v.retString(x+1)+p.retString(x)
  def replace(x: String , T: Term): Process =
  {
    new PIn(c, v, p.replace(x,T))
  }
}
case class PInk(c: Channel, v: TVar, u: Term, y: TVar, k: Int, p: Process) extends Process
{
  def retString(x: Int) = "| "*x+"PInk: "+k+"\n"+c.retString(x+1)+v.retString(x+1)+u.retString(x+1)+y.retString(x+1)+p.retString(x)
  def replace(x: String , T: Term): Process =
  {
    new PInk(c, v, u.replace(x,T), y, k, p.replace(x,T))
  }
}
case class POut(c: Channel, t: Term, p: Process) extends Process
{
  def retString(x: Int) = "| "*x+"POut:\n"+c.retString(x+1)+t.retString(x+1)+p.retString(x)
  def replace(x: String , T: Term): Process =
  {
    new POut(c, t.replace(x,T), p.replace(x,T))
  }
}
case class PIf (v: Value, pIf: Process, pElse: Process) extends Process
{
  def retString(x: Int) = "| "*x+"PIf:\n"+v.retString(x+1)+pIf.retString(x+1)+pElse.retString(x+1)
  def replace(x: String , T: Term): Process =
  {
    new PIf(v.replace(x,T), pIf.replace(x,T), pElse.replace(x,T))
  }
}
case class PNew(s: VConst, p: Process) extends Process
{
  def retString(x: Int) = "| "*x+"PNew:\n"+s.retString(x+1)+p.retString(x)
  def replace(x: String , T: Term): Process =
  {
    new PNew(s, p.replace(x,T))
  }
}
case class PSeq(l: Process, p: Process) extends Process
{
  def retString(x: Int) = "| "*x+"PSeq:\n"+l.retString(x+1)+p.retString(x+1)
  def replace(x: String , T: Term): Process =
  {
    new PSeq(l.replace(x,T), p.replace(x,T))
  }
}

