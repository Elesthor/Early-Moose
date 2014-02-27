////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [terms.scala]                                                    //
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


abstract class Term
{
  def retString (x: Int): String
  def replace (x: String, t: Term): Term
  def toString: String
}

import scala.util.matching.Regex

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////


abstract class Value
{
  def retString (x: Int): String
  def toString: String
}

case class VInt  (v: Int) extends Value
{
  def retString(x: Int): String = "| "*x+"Int:\n"+"| "*(x+1)+v.toString+"\n"
  override def toString: String = v.toString
}

case class VCount(l: Term) extends Value
{
  def retString(x: Int): String = "| "*x+"Count:\n"+l.retString(x+1)
  override def toString: String = "count("+l.toString+")"
}

case class VSup  (left: Value, right: Value) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Sup:\n"+left.retString(x+1)+right.retString(x+1)
  override def toString: String = "("+left.toString+")>("+right.toString+")"
}

case class VEqual(left: Term, right: Term) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Equal:\n"+left.retString(x+1)+right.retString(x+1)
  override def toString: String = "("+left.toString+")=("+right.toString+")"
}

case class VAnd(left: Value, right: Value) extends Value
{
  def retString(x: Int): String =
    "| "*x+"And:\n"+left.retString(x+1)+right.retString(x+1)
  override def toString: String = "("+left.toString+")/\\("+right.toString+")"
}

case class VOr (left: Value, right: Value) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Or:\n"+left.retString(x+1)+right.retString(x+1)
  override def toString: String = "("+left.toString+")\\/("+right.toString+")"
}

case class VNot (v: Value) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Not:\n" + v.retString(x+1)
  override def toString: String = "not("+v.toString+")"
}

case class VConst(s: String) extends Value
{
  def retString(x: Int): String = "| "*x+"Const:\n"+"| "*(x+1)+s+"\n"
  override def toString: String = s
}


////////////////////////////////////////////////////////////////////////////////
//                               Lists                                        //
////////////////////////////////////////////////////////////////////////////////

case class ListTerm(content: List[Term]) extends Term
{
  def retString(x: Int): String = "| "*x+"List:\n"+content.foldLeft(""){
                              (acc, item) => acc+ item.retString(x+1)}
  def replace(x: String , T: Term): ListTerm =
  {
    new ListTerm((content.map((p=>p.replace(x,T)))))
  }

  def flatten(): ListTerm =
  {
    def aux(l: List[Term]): List[Term] = l match
    {
      case Nil => Nil
      case (head: ListTerm) :: tail => aux(head.content) ++ aux(tail)
      case head :: tail => head :: aux(tail)
    }
    new ListTerm( content )
  }
  override def toString: String = content.foldLeft(""){(acc, item) => acc+"("+item.toString+")::"}.dropRight(2)
}

case class Cons(head: Term, tail: Option[Cons]) extends Term
{
  def replace(x: String , T: Term): Cons =
  {
    if (tail.isDefined)
    {
      new Cons(head.replace(x,T), Some(tail.get.replace(x,T)))
    }
    else
    {
      new Cons(head.replace(x,T), None)
    }
  }
  def toList(): ListTerm =
  {
    def aux(h: Term, t: Option[Cons]): List[Term] =
      {
        if (t.isDefined) h::aux(t.get.head, t.get.tail)
        else List(h)
      }
    new ListTerm(aux(head,tail))
  }

  def retString(x: Int): String =  ""
  override def toString: String = "("+head.toString+")"+(tail match
    {
      case None    => "[]"
      case Some(t) => "::"+t.toString
    })
}
////////////////////////////////////////////////////////////////////////////////
//                               Terms                                        //
////////////////////////////////////////////////////////////////////////////////

case class TVar (p: String) extends Term
{
  def retString(x: Int): String = "| "*x+"Var:\n"+"| "*(x+1)+p+"\n"
  def replace(x: String , T: Term): Term =
  {
    if (x==p)
    {
      return T
    }
    else
    {
      return new TVar(p)
    }
  }
  override def toString: String = p
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term
{
  def retString(x: Int): String = v.retString(x)
  def replace(x: String ,T: Term): Term = new TValue(v)
  override def toString: String = v.toString
}

case class TPair(left: Term, right: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Pair:\n"+left.retString(x+1)+right.retString(x+1)
  def replace(x: String, T: Term): Term =
    new TPair(left.replace(x,T), right.replace(x,T))
  override def toString: String = "pair("+left.toString+","+right.toString+")"
}

case class TPi1 (t: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Pi1:\n"+t.retString(x+1)
  def replace(x: String, T: Term): Term = new TPi1(t.replace(x,T))
  override def toString: String = "pi1("+t.toString+")"
}

case class TPi2 (t: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Pi2:\n"+t.retString(x+1)
  def replace(x: String, T: Term): Term = new TPi2(t.replace(x,T))
  override def toString: String = "pi2("+t.toString+")"
}

case class TEnc (left: Term, right: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Enc:\n"+left.retString(x+1)+right.retString(x+1)
  def replace(x: String, T: Term): Term =
    new TEnc(left.replace(x,T), right.replace(x,T))
  override def toString: String = "enc("+left.toString+","+right.toString+")"
}

case class TDec (left: Term, right: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Dec:\n"+left.retString(x+1)+right.retString(x+1)
  def replace(x: String, T: Term): Term =
    new TDec(left.replace(x,T), right.replace(x,T))
  override def toString: String = "dec("+left.toString+","+right.toString+")"
}

case class TPk  (v: Value) extends Term
{
  def retString(x: Int): String = "| "*x+"Pk:\n"+v.retString(x+1)
  def replace(x: String, T: Term): Term = new TPk(v)
  override def toString: String = "pk("+v.toString+")"
}

case class TSk  (v: Value) extends Term
{
  def retString(x: Int): String = "| "*x+"Sk:\n"+v.retString(x+1)
  def replace(x: String, T: Term): Term = new TSk(v)
  override def toString: String = "sk("+v.toString+")"
}

