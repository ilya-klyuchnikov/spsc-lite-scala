package spsc.multi

import spsc._
import spsc.Algebra._
import NAlgebra._

// generator of residual programs in NLanguage
class NGenerator(val tree: Tree) {

  private val sigs = scala.collection.mutable.Map[Node, (String, List[NVar])]()
  lazy val result = fold(tree.root)

  // proceed base node or repeat node by creating letrec or call respectively 
  // otherwise, delegate to make
  private def fold(n: Node): NExpr = n.fnode match {

    case None => {
      lazy val traversed = make(n)
      tree.leaves.filter(_.fnode == Some(n)) match {

        case Nil => {
          traversed
        }

        case repeatNodes => {
          val (f, vars) = createSignature(n, repeatNodes)
          sigs(n) = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = Map(vars zip newVars: _*)
          val body = nSubst(traversed, sub)
          NLet(f, NFun(f, newVars, body), NCall(f, vars))
        }

      }
    }

    case Some(fnode) => {
      val (name, args) = sigs(fnode)
      val sub = findSubst(fnode.expr, n.expr)
      val sub1 = sub map { case (k, v) => (NVar(k.name), convert(v)) }
      NCall(name, args map { nSubst(_, sub1) })
    }
  }

  private def make(n: Node): NExpr = n.expr match {
    case Var(vn) => NVar(vn)
    case Ctr(cn, _) => NCtr(cn, tree.children(n).map(fold))
    case Let(_, bs) => {
      val n0 :: ns = tree.children(n)
      val sub = Map() ++ (bs map { kv => NVar(kv._1.name) } zip (ns map fold))
      nSubst(fold(n0), sub)
    }
    case _ =>
      if (tree.children(n).head.contr == null) {
        // transient step
        fold(tree.children(n).head)
      } else {
        // variants
        val sortedChildren = tree.children(n) sortWith { (n1, n2) => (n1.contr.pat.name compareTo n1.contr.pat.name) < 0 }
        val sel = NVar(tree.children(n).head.contr.v.name)
        val bs = sortedChildren map { c => (convert(c.contr.pat), fold(c)) }
        NCase(sel, bs)
      }
  }

  private def createSignature(fNode: Node, recNodes: List[Node]): (String, List[NVar]) = {
    var fVars: List[Var] = vars(fNode.expr)

    var changedVars = Set[Var]()
    for (n <- recNodes) {
      val betaT = n.expr
      val sub = findSubst(fNode.expr, betaT)
      val args0 = sub map { p => p._1 }
      changedVars = changedVars ++ args0
    }
    fVars = fVars filter { changedVars.contains }

    (createFName(), fVars map { v => NVar(v.name) })
  }

  var fCount = 0
  private def createFName(): String = {
    fCount = fCount + 1
    "f." + fCount
  }

  var vCount = 0
  private def createVar(): NVar = {
    vCount = vCount + 1
    NVar("v." + fCount)
  }
}