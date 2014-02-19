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

import scala.util.matching.Regex

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

case class ListTerm(content: List[Term]) extends Term{
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

case class SyntaxError extends Exception


// Utilities function
def BooleanToInt (b: Boolean): Int = if (b) 1 else 0
def IntToBoolean (x: Int): Boolean = if (x==0) false else true
def isInt        (x: String): Boolean =
    try {x.toInt;true} catch{ case _: Throwable => false}
def isPositiveInt(x:String): Boolean = isInt(x) && x.toInt > 0

// Split a string "XXX(.., ..)" at the comma
// return the two arguments of the pair
def parseComma(str : String, dropped: Int):  Array[String] = {
    def crawler(s: String, acc: String, parenthesisCount: Int): Array[String] ={
        if (s.length == 0) throw new SyntaxError
        else if (s(0) == (',') && parenthesisCount == 0)
            return Array(acc.drop(dropped), s.substring(1,s.length-1))
        else if (s(0) == ('(')) crawler(s.drop(1), acc+s(0), parenthesisCount+1)
        else if (s(0) == (')')) crawler(s.drop(1),acc+s(0), parenthesisCount-1)
        else crawler(s.drop(1),acc+s(0), parenthesisCount)
    }
    return crawler(str,"", -1) // The first comma is after "XXX"
}

// Values interpretor
def interpretValue(v: Value): Int = {
  v match {
  case VInt(x)               => x
  case VCount(li)            => li match{
    // Remove everything except poisitve integers and count them
    case ListTerm (l) => ((l.map(interpretTerm)).filter(isPositiveInt)).length
  }
  case VSup  (left, right)  => BooleanToInt (interpretValue(left) >
                                                        interpretValue(right))
  case VEqual (left, right) => BooleanToInt (interpretTerm(left) ==
                                                         interpretTerm(right))
  case VAnd (left, right)   => BooleanToInt(IntToBoolean(interpretValue(left))
                                       && IntToBoolean(interpretValue(right)))
  case VOr (left, right)    => BooleanToInt(IntToBoolean(interpretValue(left))
                                      || IntToBoolean(interpretValue(right)))
  case VNot (v)             => BooleanToInt(!IntToBoolean(interpretValue(v)))
  }
}

// Term intrpretor
def interpretTerm (t: Term): String = {
  try{
    t match {
      case TVar (p)           => p
      case TValue (v)         => interpretValue(v).toString
      case TPair(left, right) =>
        "Pair("+interpretTerm(left)+","+interpretTerm(right)+")"
      case TPi1 (t)           =>
        if ((interpretTerm(t)).startsWith("pair(")) parseComma(interpretTerm(t), 5)(0)
        else throw new SyntaxError
      case TPi2 (t)           =>
        if ((interpretTerm(t)).startsWith("pair(")) parseComma(interpretTerm(t), 5)(1)
        else throw new SyntaxError
      case TEnc (left, right) =>
        "Enc("+interpretTerm(left)+","+interpretTerm(right)+")"
      case TDec (left, right) =>
        if (interpretTerm(left).matches("""enc\(.*,pk\(\d+\)\)""")) {
          val splittedLeft = parseComma(interpretTerm(left), 4);
          val splittedRight = interpretTerm(right);
          if (splittedRight.matches("""sk\(\d+\)""") &&
            (splittedLeft(1).substring(3,splittedLeft(1).length-1)).toInt==
                           (splittedRight.substring(3,splittedRight.length-1)).toInt)
              splittedLeft(0)
          else throw new SyntaxError
        }
          else throw new SyntaxError
      case TPk  (v)           => "Tpk("+interpretValue(v)+")"
      case TSk  (v)           => "Tpk("+interpretValue(v)+")"
      case ListTerm (l)       => l.map(interpretTerm).toString
    }
  }
  catch {case _: Throwable => return "err"}
}



