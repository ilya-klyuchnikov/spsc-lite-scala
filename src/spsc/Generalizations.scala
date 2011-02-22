package spsc

object Generalizations {

  type Sub = List[(Var, Term)]

  // the main trick is to remove duplicates here
  def gens(e: Term): List[Term] = e match {
    case Let(_, _) => Nil
    case Ctr(_, _) => Nil
    case _ => generalize(e, Nil).
      foldRight(List[(Term, Sub)]()) { (elem, filtered) =>
        filtered.find { x => Algebra.renaming(elem._1, x._1) } match { case None => elem :: filtered; case Some(_) => filtered }
      }.
      filter { !_._1.isInstanceOf[Var] }. // remove full abstraction
      filter { !_._2.isEmpty }. // remove identity
      map { case (t, sub) => if (sub.isEmpty) t else Let(t, sub) }
  }

  private def generalize(e: Term, sub: Sub): List[(Term, Sub)] = {

    // generalizations of subcomponents
    val xs = e match {
      case FCall(n, args) if !args.isEmpty =>
        generalizeArgs(args) map { case (args1, sub1) => (FCall(n, args1), sub1) }
      case GCall(n, args) if !args.isEmpty =>
        generalizeArgs(args) map { case (args1, sub1) => (GCall(n, args1), sub1) }
      case Ctr(n, args) if !args.isEmpty =>
        generalizeArgs(args) map { case (args1, sub1) => (Ctr(n, args1), sub1) }
      case t => List((t, sub))
    }

    // reference to already defined binding, if any
    val ys: List[(Term, Sub)] = sub find { _._2 == e } match {
      case None => Nil
      case Some((v, _)) => List((v, sub))
    }

    // new binding = full abstraction
    val ns: List[(Term, Sub)] = {
      val fv = Algebra.freshVar()
      List((fv, (fv, e) :: sub))
    }

    ys ++ ns ++ xs
  }

  private def generalizeArgs(args: List[Term]): List[(List[Term], Sub)] =
    args.foldRight(List[(List[Term], Sub)]((Nil, Nil))) { (arg, acc) =>
      for ((terms, sub) <- acc; (t, sub1) <- generalize(arg, sub)) yield (t :: terms, sub1)
    }

}