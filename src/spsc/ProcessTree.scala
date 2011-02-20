package spsc
import Algebra._

case class Contraction(v: Var, pat: Pat)

class Node(val expr: Term, val parent: Node, val contr: Contraction) {

  def ancestors: List[Node] =
    if (parent == null) Nil else parent :: parent.ancestors

  def isProcessed = expr match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case _ => fnode != null
  }

  def fnode =
    ancestors.find { n => !trivial(n.expr) && equiv(expr, n.expr) }.getOrElse(null)
}

// pure functional partial tree
// maybe it is not elegant enough, but works
class Tree(val root: Node, val children: Map[Node, List[Node]]) {

  def addChildren(n: Node, cs: List[(Term, Contraction)]) =
    new Tree(root, children + (n -> (cs map { case (t, b) => new Node(t, n, b) })))

  def replace(n: Node, exp: Term) =
    if (n == root) new Tree(new Node(exp, null, null), Map().withDefaultValue(Nil))
    else {
      val p = n.parent
      val cs = children(p) map { m => if (m == n) new Node(exp, p, n.contr) else m }
      new Tree(root, children + (p -> cs))
    }

  private def leaves_(node: Node): List[Node] =
    if (children(node).isEmpty) List(node)
    else (children(node) map leaves_).flatten

  // leaves from left to right
  def leaves() = leaves_(root)

  // left unprocessed node
  def uprocessedLeaf: Option[Node] =
    leaves find { !_.isProcessed }
}