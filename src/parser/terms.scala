////////////////////////////////////////////////////////////////////////////////
//                             [ Early Moose ]                                //
//                                                                            //
//           [terms.scala]                                                    //
//                              https://github.com/Elesthor/Early-Moose       //
////////////////////////////////////////////////////////////////////////////////
//
//
//

abstract class Term{
    def retString (x: Int): String
}

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////

abstract class Value extends Term{
    def retString (x: Int): String
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term{
     def retString(x: Int): String = " "*x+v.retString(x)
}

case class VInt  (v: Int) extends Value{
     def retString(x: Int): String = return " "*x+v.toString+"\n"
}

// case class VCount(l: ListTerm)                      extends Value{
//      def retString(x: Int): String =
//        " "*x+"Count("+l.retString(x)+")\n"
// }

case class VSup  (left: Value, right: Value) extends Value{
     def retString(x: Int): String =
        " "*x+"Sup:\n"+left.retString(x+1)+right.retString(x+1)
}

case class VEqual(left: Term, right: Term)          extends Value{
     def retString(x: Int): String =
        " "*x+"Equal:\n"+left.retString(x+1)+right.retString(x+1)
}

case class VAnd  (left: Value, right: Value) extends Value{
     def retString(x: Int): String =
        " "*x+"And:\n"+left.retString(x+1)+right.retString(x+1)
}
case class VOr   (left: Value, right: Value) extends Value{
     def retString(x: Int): String =
        " "*x+"Or:\n"+left.retString(x+1)+right.retString(x+1)
}
case class VNot  (v: Value) extends Value{
     def retString(x: Int): String =
        " "*x+"Not:\n" + v.retString(x+1)
}
case class VConst(s: String) extends Value{
     def retString(x: Int): String = " "*x+s
}


////////////////////////////////////////////////////////////////////////////////
//                               Lists                                        //
////////////////////////////////////////////////////////////////////////////////
abstract class ListTerm                                      extends Term{
    val content: Option[List[Term]] = None
}


////////////////////////////////////////////////////////////////////////////////
//                               Terms                                        //
////////////////////////////////////////////////////////////////////////////////
case class TVar (p: String) extends Term{
    def retString(x: Int): String = " "*x+p
}

case class TPair(left: Term, right: Term) extends Term{
    def retString(x: Int): String =
        " "*x+"Pair:\n"+left.retString(x+1)+right.retString(x+1)
}

case class TPi1 (t: Term) extends Term{
    def retString(x: Int): String =
        " "*x+"Pi1:\n"+t.retString(x)+"\n"
}

case class TPi2 (t: Term) extends Term{
    def retString(x: Int): String =
        " "*x+"Pi2:\n"+t.retString(x)+"\n"
}

case class TEnc (left: Term, right: Term) extends Term{
    def retString(x: Int): String =
        " "*x+"Enc:\n"+left.retString(x+1)+right.retString(x+1)
}
case class TDec (left: Term, right: Term) extends Term{
    def retString(x: Int): String =
        " "*x+"Dec:\n"+left.retString(x+1)+right.retString(x+1)
}

case class TPk  (v: Value) extends Term{
    def retString(x: Int): String = " "*x+"Pk:\n"+v.retString(x+1)+"\n"
}

case class TSk  (v: Value) extends Term{
    def retString(x: Int): String = " "*x+"Sk:\n"+v.retString(x+1)+"\n"
}



val a = new VInt(5)
val b = new VInt(1)
val x = new VConst("x")

val c = new VSup(a,b)
val d = new VAnd(a, c)

val e = new TSk(d)
val f = new VEqual(e, a)

println(f.retString(0))

