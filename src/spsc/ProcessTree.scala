package spsc
import Algebra._

case class Contraction(v: Var, pat: Pat) {
	override def toString = v + " = " + pat
}

class Node(val expr: Term, val parent: Node, val contr: Contraction) {

  def ancestors: List[Node] =
    if (parent == null) Nil else parent :: parent.ancestors

  def isProcessed = expr match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case _ => fnode.isDefined
  }

  def fnode: Option[Node] =
    ancestors.find { n => !trivial(n.expr) && renaming(expr, n.expr) }

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

  def size(n: Node): Int = 1 + children(n).map { size }.sum

  def treeSize = size(root)

  def toString(indent: String, n: Node): String = {
    val sb = new StringBuilder(indent + "|__" + n.expr)
    for (edge <- children(n)) {
      sb.append("\n  " + indent + "|" + (if (edge.contr != null) edge.contr else ""))
      sb.append("\n" + toString(indent + "  ", edge))
    }
    sb.toString
  }
  
  override def toString = toString("", root)
}