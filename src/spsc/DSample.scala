package spsc

object DSample {

  val target1 =
    "gApp(gApp(x, y), z)"
  val program1 =
    """
    gApp(Nil(), vs) = vs; 
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    """

  val target2 =
    "gApp1(gApp1(x, y), z)"
  val program2 =
    """
    gApp1(Nil(), vs) = vs; 
    gApp1(Cons(u, us), vs) = Cons(u, gApp2(us, vs));
	gApp2(Nil(), vs) = vs; 
    gApp2(Cons(u, us), vs) = Cons(u, gApp1(us, vs));
    """

  val target3 =
    "f1(a,b)"
  val program3 =
    """
      f1(a,b) = gCompare(gLength(a), gLength(b));
	  gLength(Nil()) = Z();
	  gLength(Cons(x, xs))=S(gLength(xs));
	  gCompare(Z(), y)=gComp1(y);
	  gCompare(S(x), y)=gComp2(y, x);
	  gComp1(Z())=Equal();
	  gComp1(S(y))=Less();
	  gComp2(Z(), x)=Greater();
	  gComp2(S(y), x)=gCompare(x,y);
	  """

  val target4 = "gFlip(gFlip(t))"
  val program4 =
    """
	  gFlip(Leaf(n)) = Leaf(n);
	  gFlip(Branch(t1, t2)) = Branch(gFlip(t1), gFlip(t2));
	  """

  def main(args: Array[String]): Unit = {
    runDeforester(target1, program1)
    runDeforester(target2, program2)
    runDeforester(target3, program3)
    runDeforester(target4, program4)
  }

  def runDeforester(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = Deforester.buildTree(target, Nil, program)
    println(sc)
    println()
  }

}