package spsc
import Algebra._

class ResidualProgramGenerator(val tree: Tree) {

  private val sigs = scala.collection.mutable.Map[Node, (String, List[Var])]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  lazy val result = (walk(tree.root), Program(defs.toList))

  private def walk(n: Node): Term = n.fnode match {

    case None => n.expr match {
      case Let(_, bs) => {
        val n0 :: ns = tree.children(n)
        val sub = Map() ++ (bs map { _._1 } zip (ns map walk))
        subst(walk(n0), sub)
      }
      case v: Var => v
      case Ctr(name, args) => walkCtr(n, name, args)
      case FCall(name, args) => walkCall(n, name, args)
      case GCall(name, args) => walkCall(n, name, args)
    }

    case Some(fnode) => sigs(fnode) match {
      case (name, args) =>
        if (tree.children(fnode).head.contr == null)
          subst(FCall(name, args), findSubst(fnode.expr, n.expr))
        else subst(GCall(name, args), findSubst(fnode.expr, n.expr))
    }
  }

  def walkCall(n: Node, name: String, args: List[Term]) = {
    val vs = vars(n.expr)
    if (tree.children(n).head.contr != null) {
      val (gname, _) = sigs.getOrElseUpdate(n, (rename(name, "g"), vs))
      for (cn <- tree.children(n))
        defs += GFun(gname, cn.contr.pat, vs.tail, walk(cn))
      GCall(gname, vs)
    } else if (tree.leaves.exists(_.fnode.map(_ == n).getOrElse(false))) {
      val (fname, fargs) = sigs.getOrElseUpdate(n, (rename(name, "f"), vs))
      defs += FFun(fname, fargs, walk(tree.children(n).head))
      FCall(fname, vs)
    } else walk(tree.children(n).head)
  }

  def walkCtr(n: Node, name: String, args: List[Term]): Term =
    if (tree.leaves.exists(_.fnode.map(_ == n).getOrElse(false))) {
      val vs = vars(n.expr)
      val (fname, fargs) = sigs.getOrElseUpdate(n, (rename(name, "f"), vs))
      defs += FFun(fname, fargs, Ctr(name, tree.children(n).map(walk)))
      FCall(fname, vs)
    } else {
      Ctr(name, tree.children(n).map(walk))
    }

  def rename(f: String, b: String) = { b + f.drop(1) + (sigs.size + 1) }
}