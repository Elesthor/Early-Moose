////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [terms.scala]                                                    //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//

abstract class Term

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////

class Value()                                       extends Term

// Binding class between Term and value to have a case class.
case class TValue (v: Value)                        extends Term{
    override def toString(): String = v.toString
}

case class VInt  (v: Int)                           extends Value{
    override def toString(): String = v.toString+"\n"
}
case class VCount(l: ListTerm)                      extends Value{
    override def toString(): String =
        "Count("+l.toString+")\n"
}
case class VSup  (left: Value, right: Value)        extends Value{
    override def toString(): String =
        "Sup:\n"+left.toString+right.toString+"\n"
}
case class VEqual(left: Term, right: Term)          extends Value{
    override def toString(): String =
        "Equal:\n"+left.toString+right.toString+"\n"
}
case class VAnd  (left: Value, right: Value)        extends Value{
    override def toString(): String =
        "And:\n"+left.toString+right.toString+"\n"
}
case class VOr   (left: Value, right: Value)        extends Value{
    override def toString(): String =
        "Or:\n"+left.toString+right.toString+"\n"
}
case class VNot  (v: Value)                         extends Value{
    override def toString(): String = "Not:\n" + v.toString
}
case class VConst(s: String)                        extends Value{
    override def toString(): String = s
}

val a = new VInt(5)
val b = new VInt(1)

val c = new VSup(a,b)
val d = new VEqual(a, c)

val toto = new TValue(d)
println(toto.toString)
////////////////////////////////////////////////////////////////////////////////
//                               Lists                                        //
////////////////////////////////////////////////////////////////////////////////
class ListTerm                                      extends Term{
    val content: Option[List[Term]] = None
}


////////////////////////////////////////////////////////////////////////////////
//                               Terms                                        //
////////////////////////////////////////////////////////////////////////////////
case class TVar (x: String)                         extends Term
case class TPair(left: Term, right: Term)           extends Term
case class TPi1 (t: Term)                           extends Term
case class TPi2 (t: Term)                           extends Term
case class TEnc (left: Term, right: Term)           extends Term
case class TDec (left: Term, right: Term)           extends Term
case class TPk  (v: Value)                          extends Term
case class TSk  (v: Value)                          extends Term
