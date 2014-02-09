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
}

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////


abstract class Value{
    def RetString (x: Int): String
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term{
     def RetString(x: Int): String = v.RetString(x)
}

case class VInt  (v: Int) extends Value{
     def RetString(x: Int): String = " "*x+v.toString+"\n"
}

case class VCount(l: ListTerm) extends Value{
      def RetString(x: Int): String = " "*x+"Count("+l.RetString(x)+")\n"
}

case class VSup  (left: Term, right: Term) extends Value{
     def RetString(x: Int): String =
        " "*x+"Sup:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VEqual(left: Term, right: Term) extends Value{
     def RetString(x: Int): String =
        " "*x+"Equal:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VAnd  (left: Term, right: Term) extends Value{
     def RetString(x: Int): String =
        " "*x+"And:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VOr   (left: Term, right: Term) extends Value{
     def RetString(x: Int): String =
        " "*x+"Or:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class VNot  (v: Term) extends Value{
     def RetString(x: Int): String =
        " "*x+"Not:\n" + v.RetString(x+1)
}

case class VConst(s: String) extends Value{
     def RetString(x: Int): String = " "*x+s
}


////////////////////////////////////////////////////////////////////////////////
//                               Lists                                        //
////////////////////////////////////////////////////////////////////////////////

class ListTerm(content: List[Term]) extends Term{
    def RetString(x: Int): String = " "*x+"\n"+"List"+content.foldLeft(""){
                                (acc, item) => acc+ item.RetString(x+1)+"\n"}
}

////////////////////////////////////////////////////////////////////////////////
//                               Terms                                        //
////////////////////////////////////////////////////////////////////////////////

case class TVar (p: String) extends Term{
    def RetString(x: Int): String = " "*x+p
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term{
     def RetString(x: Int): String = " "*x+v.RetString(x)
}

case class TPair(left: Term, right: Term) extends Term{
    def RetString(x: Int): String =
        " "*x+"Pair:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class TPi1 (t: Term) extends Term{
    def RetString(x: Int): String =
        " "*x+"Pi1:\n"+t.RetString(x)+"\n"
}

case class TPi2 (t: Term) extends Term{
    def RetString(x: Int): String =
        " "*x+"Pi2:\n"+t.RetString(x)+"\n"
}

case class TEnc (left: Term, right: Term) extends Term{
    def RetString(x: Int): String =
        " "*x+"Enc:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class TDec (left: Term, right: Term) extends Term{
    def RetString(x: Int): String =
        " "*x+"Dec:\n"+left.RetString(x+1)+right.RetString(x+1)
}

case class TPk  (v: Term) extends Term{
    def RetString(x: Int): String = " "*x+"Pk:\n"+v.RetString(x+1)+"\n"
}

case class TSk  (v: Term) extends Term{
    def RetString(x: Int): String = " "*x+"Sk:\n"+v.RetString(x+1)+"\n"
}


////////////////////////////////////////////////////////////////////////////////
//                              Interpretor                                   //
////////////////////////////////////////////////////////////////////////////////

def BooleanToInt (b: Boolean): Int = if (b) 1 else 0
def IntToBoolean (x: Int): Boolean = if (x==0) false else true

def interpretValue(v: Value): Int = v match {
    case VInt(x)                => x
    case VCount(l)              => 0
    case VSup  (left, right)    => BooleanToInt (interpretValue(left) >
                                                          interpretValue(right))
    case VEqual (left, right)   => BooleanToInt (left == right)
    case VAnd (left, right)     => BooleanToInt(IntToBoolean(interpretValue(left))
                                         && IntToBoolean(interpretValue(right)))
    case VOr (left, right)      => BooleanToInt(IntToBoolean(interpretValue(left))
                                         || IntToBoolean(interpretValue(right)))
    case VNot (v)               => BooleanToInt(!IntToBoolean(interpretValue(v)))
}

case class Pair(left: String, right: String){
    override def  toString(): String = {"("+left+","+"right"+")"}
}

def interpretTerm (t: Term): String = t match{
    case TVar (p)           => p
    case TValue (v)         => interpretValue(v).toString
    case TPair(left, right) => {
        val x = new Pair(interpretTerm(left), interpretTerm (right))
        x.toString
    }
    case TPi1 (t1)          => interpretTerm(t1) match {
        case Pair(left, right) => left
        case _ => throw new Exception
    }
    case TPi2 (t1)          => interpretTerm(t1) match {
        case Pair(left, right) => right
        case _ => throw new Exception
    }
    case TEnc (left, right) => "Enc("+interpretTerm(left)+","+ interpretTerm(right)+")"
    case TDec (left, right) => "Dec("+interpretTerm(left)+","+ interpretTerm(right)+")"
    case TPk  (v)           => "Tpk("+interpretValue(v)+")"
    case TSk  (v)           => "Tsk("+interpretValue(v)+")"

}

val a = new VInt(5)
val b = new VInt(0)
val x = new VConst("x")

val c = new VSup(b,a)
val d = new VSup(a,b)

val e = new VAnd(c,d)
// val d = new VAnd(a, c)

// val e = new TSk(d)
// val f = new VEqual(e, a)
// val g = new ListTerm(Some(List(a,f,x)))

println(interpretValue(e))

