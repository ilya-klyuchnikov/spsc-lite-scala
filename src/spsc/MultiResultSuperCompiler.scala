package spsc

import Algebra._
import Decomposition._

class MultiResultSuperCompiler(p: Program) extends BaseSuperCompiler(p) {

  def buildTrees(e: Term): List[Tree] = {
    var t = new Tree(new Node(e, null, null), Map().withDefaultValue(Nil))
    buildTrees(t)
  }

  def drive(t: Tree, n: Node): List[Tree] =
    List(t.addChildren(n, driveExp(n.expr)))

  def generalize(t: Tree, n: Node): List[Tree] =
    splits(n.expr) map { t.replace(n, _) }

  def splits(t: Term): List[Term] = t match {
    case call@FCall(n, args) if vars(call) != args => {
      val vs = args map freshVar
      List(Let(FCall(n, vs), vs zip args))
    }
    case call@GCall(n, args) if vars(call) != args => {
      val vs = args map freshVar
      List(Let(GCall(n, vs), vs zip args))
    }
    case _ => Nil
  }

  def accept(t: Tree, n: Node): Boolean =
    n.expr.size < 20

  def buildTrees(t: Tree): List[Tree] =
    t.uprocessedLeaf match {
      case Some(n) if (accept(t, n)) =>
        (drive(t, n) ++ generalize(t, n)).map { buildTrees }.flatten
      case None =>
        List(t)
      case _ =>
        List()
    }
}