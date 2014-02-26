object App
{
  def main(args: Array[String])
  {
    val t = new Interpretor()

    val program = (new Parser(new InputFromString("out(c1,test1).out(c1,test2).in^2(c1,x ->x as y).out(stdout, y)"))).Parse()
    
    val std = new Channel("stdout")
    val triv = new PTrivial()
    val c1 = new Channel("c1")
    val test2 = new PInk (c1, new TVar("x"), new TVar("x"), new TVar("y"), 2,new POut(std, new TVar("y"), triv))
    val test3 = new POut(c1,new TVar("test1"), new POut(c1,new TVar("test2"), test2))
    val a = new MetaProc(test3, 1, None)
    println(a.RetString(0))
    t.InterpretMetaProc(a)
  }
}
