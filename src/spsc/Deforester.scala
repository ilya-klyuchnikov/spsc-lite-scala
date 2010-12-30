package spsc

import Decomposition._
import Algebra._

abstract sealed class PTree[A] {
  def toString(indent: String): String
}
case class Leaf[A](expr: A) extends PTree[A] {
  override def toString(): String = expr.toString
  def toString(indent: String): String = indent + toString()
}
case class Fork[A](expr: A, children: List[PTree[A]]) extends PTree[A] {
  override def toString(): String = toString("")
  def toString(indent: String): String = {
    indent + expr.toString + children.map { "\n" + _.toString(indent + " ") }.mkString("")
  }
}

// in haskell tree can be lazy!!
object Deforester {
  def buildTree(e: Term, history: List[Term], program: Program): PTree[Term] = {
    if (canFold(e, history)) {
      Leaf(e)
    } else {
      Fork(e, Driver.drive(e, program) map { buildTree(_, e :: history, program) })
    }
  }

  private def canFold(e: Term, history: List[Term]) = history.exists { SimpleAlgebra.renaming(_, e, Nil).isDefined }
  private def driveStep(e: Term): List[Term] = null
}

object Driver {
  def drive(e: Term, p: Program): List[Term] = decompose(e) match {
    case ObservableVar(v) => Nil
    case ObservableCtr(Ctr(_, args)) => args
    case context@Context(red) =>
      red match {
        case RedexFCall(FCall(name, args)) => {
          val fReduced = subst(p.f(name).term, Map(p.f(name).args.zip(args): _*))
          List(context.replaceRedex(fReduced))
        }
        case RedexGCallCtr(GCall(name, args), Ctr(cname, cargs)) => {
          val g = p.g(name, cname)
          val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
          List(context.replaceRedex(gReduced))
        }
        // TODO!!!
        case RedexGCallVar(GCall(name, args), v) => {
          p.gs(name) map { g =>
            val fp = freshPat(g.p)
            val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
            context.replaceRedex(gReduced)
          }
        }
      }
  }

  def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}

object SimpleAlgebra {
  // this is not good - it doesn't take into account deconstructors
  def renaming(e1: Term, e2: Term, walked: List[(Var, Var)]): Option[List[(Var, Var)]] = {
    (e1, e2) match {
      case (v1: Var, v2: Var) =>
        lookup(v1, walked) match {
          case None => Some((v1, v2) :: walked)
          case Some(v3) => if (v2 == v3) Some(walked) else None
        }
      case (Ctr(n1, args1), Ctr(n2, args2)) if n1 == n2 =>
        (args1 zip args2).foldLeft(Some(walked): Option[List[(Var, Var)]]) { case (opt, (x1, x2)) => opt.flatMap { x => renaming(x1, x2, x) } }
      case (FCall(n1, args1), FCall(n2, args2)) if n1 == n2 =>
        (args1 zip args2).foldLeft(Some(walked): Option[List[(Var, Var)]]) { case (opt, (x1, x2)) => opt.flatMap { x => renaming(x1, x2, x) } }
      case (GCall(n1, args1), GCall(n2, args2)) if n1 == n2 =>
        (args1 zip args2).foldLeft(Some(walked): Option[List[(Var, Var)]]) { case (opt, (x1, x2)) => opt.flatMap { x => renaming(x1, x2, x) } }
      case _ => None
    }
  }

  def sub(e1: Term, e2: Term, walked: List[(Var, Term)]): Option[List[(Var, Term)]] = {
    (e1, e2) match {
      case (v1: Var, v2) =>
        lookup(v1, walked) match {
          case None => Some((v1, v2) :: walked)
          case Some(v3) => if (v2 == v3) Some(walked) else None
        }
      case (Ctr(n1, args1), Ctr(n2, args2)) if n1 == n2 =>
        (args1 zip args2).foldLeft(Some(walked): Option[List[(Var, Term)]]) { case (opt, (x1, x2)) => opt.flatMap { x => sub(x1, x2, x) } }
      case (FCall(n1, args1), FCall(n2, args2)) if n1 == n2 =>
        (args1 zip args2).foldLeft(Some(walked): Option[List[(Var, Term)]]) { case (opt, (x1, x2)) => opt.flatMap { x => sub(x1, x2, x) } }
      case (GCall(n1, args1), GCall(n2, args2)) if n1 == n2 =>
        (args1 zip args2).foldLeft(Some(walked): Option[List[(Var, Term)]]) { case (opt, (x1, x2)) => opt.flatMap { x => sub(x1, x2, x) } }
      case _ => None
    }
  }

  def lookup[A](v: Var, renaming: List[(Var, A)]): Option[A] =
    renaming.find { case (v1, e) => v1 == v }.map { case (v1, e1) => e1 }
}