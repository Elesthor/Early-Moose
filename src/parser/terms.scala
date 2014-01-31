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

abstract class Value                            extends Term

case class VInt(v: Int)                         extends Value
case class VCount(l: ListTerm)                  extends Value
case class VSup(left: Value, right: Value)      extends Value
case class VEqual(left: Term, right: Term)      extends Value
case class VAnd(left: Value, right: Value)      extends Value
case class VOr(left: Value, right: Value)       extends Value
case class VNot(v: Value)                       extends Value
case class VConst(s: String)                    extends Value

abstract class ListTerm                         extends Term{
    val content: Option[List[Term]]
}

case class TVar(x: String)                      extends Term
case class TPair(left: Term, right: Term)       extends Term
case class TPi1(t: Term)                        extends Term
case class TPi2(t: Term)                        extends Term
case class TEnc(left: Term, right: Term)        extends Term
case class TDec(left: Term, right: Term)        extends Term
case class TPk(v: Value)                        extends Term
case class TSk(v: Value)                        extends Term

