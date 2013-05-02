package spsc

// stack decomposition
object StackDec {
  type StackTerm = List[Term]

  def decompose(t: Term): StackTerm = t match {
    case c: Ctr =>
      c :: Nil
    case f: FCall =>
      f :: Nil
    case g@GCall(_, Ctr(_, _) :: _) =>
      g :: Nil
    case g@GCall(gn, head :: tail) =>
      val c = GCall(gn, Var("_") :: tail)
      decompose(head) :+ c
  }

}

object StackEval {
  import StackDec._
  import Algebra._

  def eval(t: Term, p: Program): Term = {
    val stack = decompose(t)
    eval_(stack, p)
  }

  def eval_(s: StackTerm, p: Program): Term = s match {
    case (c: Ctr) :: Nil =>
      c
    case (c: Ctr) :: t :: ts =>
      val t1 = subst(t, Map(Var("_") -> c))
      eval_(t1 :: ts, p)
    case FCall(name, args) :: ts =>
      val t1 = subst(p.f(name).term, Map(p.f(name).args.zip(args): _*))
      val t2 = decompose(t1)
      eval_(t2 ::: ts, p)
    case GCall(name, (Ctr(cname, cargs) :: args)) :: ts =>
      val g = p.g(name, cname)
      val t1 = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args): _*))
      val t2 = decompose(t1)
      eval_(t2 ::: ts, p)
  }

  def eval(tText: String, pText: String): Term = {
    val program = SParsers.parseProg(pText)
    val term = SParsers.parseTerm(tText)
    eval(term, program)
  }
}

object StackDecSamples extends App {
  def sampleDec(in: String) {
    val term = SParsers.parseTerm(in)
    val stack = StackDec.decompose(term)
    println(s"$term => ${stack.mkString(" | ")}")
  }

  sampleDec("g1(Nil())")
  sampleDec("g1(g2(f1(Nil())))")
}

object StackEvalSamples extends App {

  val program =
    """
    gApp(Nil(), vs) = vs;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));
    gFin1(Z()) = True();
    gFin1(S(x)) = gFin1(x);
    fFin2(x) = gFin3(gFin1(x), x);
    gFin3(False(), x) = False();
    gFin3(True(), x) = gFin4(x);
    gFin4(Z()) = True();
    gFin4(S(x)) = fFin2(x);
    """

  def eval(tText: String, pText: String) {
    val out = StackEval.eval(tText, pText)
    println(s"$tText => $out")
  }

  eval("gRev(Cons(A(), Nil()))", program)
  eval("gRev(Cons(A(), Cons(B(), Nil())))", program)
  eval("fFin2(Z())", program)
  eval("fFin2(S(S(S(Z()))))", program)
}
