object App
{
  def main(args: Array[String])
  {
    val t = new Interpretor()

    val program = (new Parser(new InputFromString("out(c1,test1).out(c1,test2).in^2(c1,x ->x as y).out(stdout, y)"))).Parse()
    t.InterpretMetaProc(program)
  }
}
