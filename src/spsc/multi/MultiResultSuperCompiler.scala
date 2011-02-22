package spsc.multi

import spsc.Algebra._
import spsc._

class MultiResultSuperCompiler(whistle: Whistle, p: Program) extends BaseSuperCompiler(p) {

  def buildTrees(e: Term): List[Tree] = {
    var t = new Tree(new Node(e, null, null), Map().withDefaultValue(Nil))
    buildTrees(t)
  }

  def drive(t: Tree, n: Node): List[Tree] =
    if (whistle.accept(t, n))
      List(t.addChildren(n, driveExp(n.expr)))
    else
      Nil

  // here we do not allow two immediate sequential generalizations
  // since they may be merged into one
  def generalize(t: Tree, n: Node): List[Tree] =
    if (n.parent == null)
      Generalizations.gens(n.expr) map { t.replace(n, _) }
    else (n.parent.expr) match {
      case Let(_, _) => Nil
      case _ => Generalizations.gens(n.expr) map { t.replace(n, _) }
    }

  var n = 0

  def buildTrees(t: Tree): List[Tree] =
    t.uprocessedLeaf match {
      case Some(n) =>
        (drive(t, n) ++ generalize(t, n)).map { buildTrees }.flatten
      case None => {
        List(t)
      }
    }
}