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

import perso.utils.NetworkTools._

abstract class Term
{
  def retString (x: Int): String
  def replace (x: String, t: Term): Term
  def toString: String
}

////////////////////////////////////////////////////////////////////////////////
//                               Values                                       //
////////////////////////////////////////////////////////////////////////////////


abstract class Value
{
  def retString (x: Int): String
  def replace (x: String, t: Term): Value
  def toString: String
}

case class VInt  (v: Long) extends Value
{
  def retString(x: Int): String = "| "*x+"Int:\n"+"| "*(x+1)+v.toString+"\n"
  def replace (x: String, t: Term): Value = this
  override def toString: String = v.toString
}

case class VCount(l: Term) extends Value
{
  def retString(x: Int): String = "| "*x+"Count:\n"+l.retString(x+1)
  def replace (x: String, t: Term): Value = new VCount(l.replace(x, t))
  override def toString: String = "count("+l.toString+")"
}

case class VSup  (left: Term, right: Term) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Sup:\n"+left.retString(x+1)+right.retString(x+1)
  def replace (x: String, t: Term): Value = new VSup(left.replace(x, t), right.replace(x, t))
  override def toString: String = "("+left.toString+")>("+right.toString+")"
}

case class VEqual(left: Term, right: Term) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Equal:\n"+left.retString(x+1)+right.retString(x+1)
  def replace (x: String, t: Term): Value = new VEqual(left.replace(x, t), right.replace(x, t))
  override def toString: String = "("+left.toString+")=("+right.toString+")"
}

case class VAnd(left: Term, right: Term) extends Value
{
  def retString(x: Int): String =
    "| "*x+"And:\n"+left.retString(x+1)+right.retString(x+1)
  def replace (x: String, t: Term): Value = new VAnd(left.replace(x, t), right.replace(x, t))
  override def toString: String = "("+left.toString+")/\\("+right.toString+")"
}

case class VOr (left: Term, right: Term) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Or:\n"+left.retString(x+1)+right.retString(x+1)
  def replace (x: String, t: Term): Value = new VOr(left.replace(x, t), right.replace(x, t))
  override def toString: String = "("+left.toString+")\\/("+right.toString+")"
}

case class VNot (v: Term) extends Value
{
  def retString(x: Int): String =
    "| "*x+"Not:\n" + v.retString(x+1)
  def replace (x: String, t: Term): Value = new VNot(v.replace(x, t))
  override def toString: String = "not("+v.toString+")"
}

case class VConst(s: String) extends Value
{
  def retString(x: Int): String = "| "*x+"Const:\n"+"| "*(x+1)+s+"\n"
  def replace (x: String, t: Term): Value = return this
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
      return this
    }
  }
  override def toString: String = p
}

// Binding class between Term and value to have a case class.
case class TValue (v: Value) extends Term
{
  def retString(x: Int): String = v.retString(x)
  def replace(x: String ,T: Term): Term = new TValue(v.replace(x,T))
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

case class TEnc (left: Term, right: Term, seed: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Enc:\n"+left.retString(x+1)+right.retString(x+1)+seed.retString(x+1)
  def replace(x: String, T: Term): Term =
    new TEnc(left.replace(x,T), right.replace(x,T), seed.replace(x,T))
  override def toString: String = "enc("+left.toString+","+right.toString+","+seed.toString+")"
}

case class TDec (left: Term, right: Term) extends Term
{
  def retString(x: Int): String =
        "| "*x+"Dec:\n"+left.retString(x+1)+right.retString(x+1)
  def replace(x: String, T: Term): Term =
    new TDec(left.replace(x,T), right.replace(x,T))
  override def toString: String = "dec("+left.toString+","+right.toString+")"
}

case class TPk  (v: Term, crypto: String) extends Term
{
  def retString(x: Int): String = "| "*x+"Pk:\n"+v.retString(x+1)+"| "*(x+1)+crypto
  def replace(x: String, T: Term): Term = new TPk(v.replace(x,T), crypto)
  override def toString: String = "pk("+v.toString+","+crypto+")"
}

case class TSk  (v: Term, crypto: String) extends Term
{
  def retString(x: Int): String = "| "*x+"Sk:\n"+v.retString(x+1)+"| "*(x+1)+crypto
  def replace(x: String, T: Term): Term = new TSk(v.replace(x,T), crypto)
  override def toString: String = "sk("+v.toString+","+crypto+")"
}

case class TRaw  (content: String) extends Term
{
  def retString(x: Int): String = "| "*x+"Raw:\n"+"| "*(x+1)+content
  def replace(x: String, T: Term): Term = this
  override def toString: String = "raw(" +
    arrayToNetwork(injectiveString(hostToArray(content))) + ")"
}

case class TOpenEnc  (v: Term) extends Term
{
  def retString(x: Int): String = "| "*x+"OpenEnc:\n"+v.retString(x+1)
  def replace(x: String, T: Term): Term = new TOpenEnc(v.replace(x,T))
  override def toString: String = "openEnc("+v.toString+")"
}

