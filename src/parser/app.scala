object App
{
  def main(args: Array[String])
  {
    val t = new Interpretor()


    val std = new Channel("stdout")
    val triv = new PTrivial()
    val c1 = new Channel("c1")
    val test2 = new PInk (c1, new TVar("x"), new TVar("x"), new TVar("y"), 2,new POut(std, new TVar("y"), triv))
    val test3 = new POut(c1,new TVar("test1"), new POut(c1,new TVar("test2"), test2))
    val a = new MetaProc(test3, 1, None)
    t.InterpretMetaProc(a)
  }
}
