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

import scala.util.matching.Regex

abstract class Term{
    def RetString (x: Int): String
}

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////


abstract class Value{
    def RetString (x: Int): String
}

case class VInt  (v: Int) extends Value{
     def RetString(x: Int): String = "| "*x+"Int:\n"+"| "*(x+1)+v.toString+"\n"
}

case class VCount(l: ListTerm) extends Value{
      def RetString(x: Int): String = "| "*x+"Count:\n"+l.RetString(x+1)
}

case class VSup  (left: Value, right: Value) extends Value{
     def RetString(x: Int): String =
        "| "*x+"Sup:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VEqual(left: Term, right: Term) extends Value{
     def RetString(x: Int): String =
        "| "*x+"Equal:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VAnd  (left: Value, right: Value) extends Value{
     def RetString(x: Int): String =
        "| "*x+"And:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VOr   (left: Value, right: Value) extends Value{
     def RetString(x: Int): String =
        "| "*x+"Or:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VNot  (v: Value) extends Value{
     def RetString(x: Int): String =
        "| "*x+"Not:\n" + v.RetString(x+1)
}

case class VConst(s: String) extends Value{
     def RetString(x: Int): String = "| "*x+"Const:\n"+"| "*(x+1)+s+"\n"
}


////////////////////////////////////////////////////////////////////////////////
//                               Lists                                        //
////////////////////////////////////////////////////////////////////////////////

class ListTerm(content: List[Term]) extends Term{
    def RetString(x: Int): String = "| "*x+"List:\n"+content.foldLeft(""){
                                (acc, item) => acc+ item.RetString(x+1)}
}

////////////////////////////////////////////////////////////////////////////////
//                               Terms                                        //
////////////////////////////////////////////////////////////////////////////////

case class TVar (p: String) extends Term{
    def RetString(x: Int): String = "| "*x+"Var:\n"+"| "*(x+1)+p+"\n"
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term{
     def RetString(x: Int): String = " "*x+v.RetString(x)
}

case class TPair(left: Term, right: Term) extends Term{
    def RetString(x: Int): String =
        "| "*x+"Pair:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class TPi1 (t: Term) extends Term{
    def RetString(x: Int): String =
        "| "*x+"Pi1:\n"+t.RetString(x+1)
}

case class TPi2 (t: Term) extends Term{
    def RetString(x: Int): String =
        "| "*x+"Pi2:\n"+t.RetString(x+1)
}

case class TEnc (left: Term, right: Term) extends Term{
    def RetString(x: Int): String =
        "| "*x+"Enc:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class TDec (left: Term, right: Term) extends Term{
    def RetString(x: Int): String =
        "| "*x+"Dec:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class TPk  (v: Value) extends Term{
    def RetString(x: Int): String = "| "*x+"Pk:\n"+v.RetString(x+1)
}

case class TSk  (v: Value) extends Term{
    def RetString(x: Int): String = "| "*x+"Sk:\n"+v.RetString(x+1)
}


////////////////////////////////////////////////////////////////////////////////
//                              Interpretor                                   //
////////////////////////////////////////////////////////////////////////////////

// def BooleanToInt (b: Boolean): Int = if (b) 1 else 0
// def IntToBoolean (x: Int): Boolean = if (x==0) false else true

// def interpretValue(v: Term): Int = v match {
//     case TValue(va) => va match{
//     case VInt(x)              => x
//     case VCount(l)            => 0
//     case VSup  (left, right)  => BooleanToInt (interpretTerm(left) >
//                                                         interpretTerm(right))
//     case VEqual (left, right) => BooleanToInt (left == right)
//     case VAnd (left, right)   => BooleanToInt(IntToBoolean(interpretTerm(left))
//                                        && IntToBoolean(interpretTerm(right)))
//     case VOr (left, right)    => BooleanToInt(IntToBoolean(interpretTerm(left))
//                                        || IntToBoolean(interpretTerm(right)))
//     case VNot (v)             => BooleanToInt(!IntToBoolean(interpretTerm(v)))
//     }
//     _ => throw new Exception
// }


// def interpretTerm (t: Term): String = t match{
//     case TVar (p)           => p
//     case TValue (v)         => interpretValue(v).toString
//     case TPair(left, right) =>
//         "Pair("+left.interpretTerm()+","+right.interpretTerm()+")"

//     case TPi1 (t1) =>
//         if (t1.matches("""Pair\(.+(,){1}.+\)""".r)) t1.split("""\(|,|,\)""")(1)
//         else throw new Exception

//     case TPi2 (t1)          =>
//         if (t1.matches("""Pair\(.+(,){1}.+\)""".r)) t1.split("""\(|,|,\)""")(2)
//         else throw new Exception

//     case TEnc (left, right) => "Enc("+interpretTerm(left)+","+ interpretTerm(right)+")"
//     case TDec (left, right) => "Dec("+interpretTerm(left)+","+ interpretTerm(right)+")"
//     case TPk  (v)           => "Tpk("+interpretValue(v)+")"
//     case TSk  (v)           => "Tsk("+interpretValue(v)+")"

// }



// val a = new TValue (new VInt(5))
// val b = new TValue (new VInt(0))
// val x = new TValue (new VConst("x"))

// val c = new TValue (new VSup(b,a))
// val d = new TValue (new VSup(a,b))

// val e = new TValue (new VAnd(c,d))

// // val d = new VAnd(a, c)

// // val e = new TSk(d)
// // val f = new VEqual(e, a)
// // val g = new ListTerm(Some(List(a,f,x)))

// println(interpretTerm(e))

