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


abstract class Term{
    def RetString (x: Int): String
    def Replace (x: String, t: Term): Term
}

import scala.util.matching.Regex

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////


abstract class Value{
    def RetString (x: Int): String
}

case class VInt  (v: Int) extends Value
{
     def RetString(x: Int): String = "| "*x+"Int:\n"+"| "*(x+1)+v.toString+"\n"
}

case class VCount(l: Term) extends Value
{
      def RetString(x: Int): String = "| "*x+"Count:\n"+l.RetString(x+1)
}

case class VSup  (left: Value, right: Value) extends Value
{
     def RetString(x: Int): String =
        "| "*x+"Sup:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VEqual(left: Term, right: Term) extends Value
{
     def RetString(x: Int): String =
        "| "*x+"Equal:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VAnd(left: Value, right: Value) extends Value
{
     def RetString(x: Int): String =
        "| "*x+"And:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VOr (left: Value, right: Value) extends Value
{
     def RetString(x: Int): String =
        "| "*x+"Or:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VNot (v: Value) extends Value
{
     def RetString(x: Int): String =
        "| "*x+"Not:\n" + v.RetString(x+1)
}

case class VConst(s: String) extends Value
{
     def RetString(x: Int): String = "| "*x+"Const:\n"+"| "*(x+1)+s+"\n"
}


////////////////////////////////////////////////////////////////////////////////
//                               Lists                                        //
////////////////////////////////////////////////////////////////////////////////

case class ListTerm(content: List[Term]) extends Term
{
    def RetString(x: Int): String = "| "*x+"List:\n"+content.foldLeft(""){
                                (acc, item) => acc+ item.RetString(x+1)}
    def Replace(x: String , T: Term): ListTerm =
    {
      new ListTerm((content.map((p=>p.Replace(x,T)))))
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
}

case class Cons(head: Term, tail: Option[Cons]) extends Term
{
  def Replace(x: String , T: Term): Cons =
  {
    if (tail.isDefined)
    {
      new Cons(head.Replace(x,T), Some(tail.get.Replace(x,T)))
    }
    else
    {
      new Cons(head.Replace(x,T), None)
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

  def RetString(x: Int): String =  ""
}
////////////////////////////////////////////////////////////////////////////////
//                               Terms                                        //
////////////////////////////////////////////////////////////////////////////////

case class TVar (p: String) extends Term
{
    def RetString(x: Int): String = "| "*x+"Var:\n"+"| "*(x+1)+p+"\n"
    def Replace(x: String , T: Term): Term =
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
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term
{
  def RetString(x: Int): String = v.RetString(x)
  def Replace(x: String ,T: Term): Term = new TValue(v)
}

case class TPair(left: Term, right: Term) extends Term
{
  def RetString(x: Int): String =
        "| "*x+"Pair:\n"+left.RetString(x+1)+right.RetString(x+1)
  def Replace(x: String, T: Term): Term =
    new TPair(left.Replace(x,T), right.Replace(x,T))
}

case class TPi1 (t: Term) extends Term
{
  def RetString(x: Int): String =
        "| "*x+"Pi1:\n"+t.RetString(x+1)
  def Replace(x: String, T: Term): Term = new TPi1(t.Replace(x,T))
}

case class TPi2 (t: Term) extends Term
{
  def RetString(x: Int): String =
        "| "*x+"Pi2:\n"+t.RetString(x+1)
  def Replace(x: String, T: Term): Term = new TPi2(t.Replace(x,T))
}

case class TEnc (left: Term, right: Term) extends Term
{
  def RetString(x: Int): String =
        "| "*x+"Enc:\n"+left.RetString(x+1)+right.RetString(x+1)
  def Replace(x: String, T: Term): Term =
    new TEnc(left.Replace(x,T), right.Replace(x,T))
}

case class TDec (left: Term, right: Term) extends Term
{
  def RetString(x: Int): String =
        "| "*x+"Dec:\n"+left.RetString(x+1)+right.RetString(x+1)
  def Replace(x: String, T: Term): Term =
    new TDec(left.Replace(x,T), right.Replace(x,T))
}

case class TPk  (v: Value) extends Term
{
  def RetString(x: Int): String = "| "*x+"Pk:\n"+v.RetString(x+1)
  def Replace(x: String, T: Term): Term = new TPk(v)
}

case class TSk  (v: Value) extends Term
{
  def RetString(x: Int): String = "| "*x+"Sk:\n"+v.RetString(x+1)
  def Replace(x: String, T: Term): Term = new TSk(v)

}

