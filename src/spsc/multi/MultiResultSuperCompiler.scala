package spsc.multi

import spsc.Algebra._
import spsc._

class MultiResultSuperCompiler(p: Program) extends BaseSuperCompiler(p) {

  def buildTrees(e: Term): List[Tree] = {
    var t = new Tree(new Node(e, null, null), Map().withDefaultValue(Nil))
    buildTrees(t)
  }

  def drive(t: Tree, n: Node): List[Tree] =
    List(t.addChildren(n, driveExp(n.expr)))

  def generalize(t: Tree, n: Node): List[Tree] = Generalizations.gens(n.expr) map { t.replace(n, _) }

  def accept(t: Tree, n: Node): Boolean =
    n.expr.size < 7

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