package spsc.multi

import spsc._

object NAlgebra {
  def nSubst(e: NExpr, m: Map[NVar, NExpr]): NExpr = e match {
    case v: NVar =>
      m.getOrElse(v, v)
    case NCtr(n, args) =>
      NCtr(n, args map { nSubst(_, m) })
    case NCall(n, args) =>
      NCall(n, args map { nSubst(_, m) })
    case NCase(sel, bs) =>
      NCase(nSubst(sel, m), bs map { case (p, e) => (p, nSubst(e, m -- p.args)) })
    case NLet(n, NFun(_, args, e), e1) =>
      NLet(n, NFun(n, args, nSubst(e, m -- args)), nSubst(e1, m))
  }

  def convert(t: Term): NExpr = t match {
    case Var(n) => NVar(n)
    case Ctr(n, args) => NCtr(n, args map convert)
    case FCall(n, args) => NCall(n, args map convert)
    case GCall(n, args) => NCall(n, args map convert)
  }

  def convert(p: Pat): NPat =
    NPat(p.name, p.args map { v => NVar(v.name) })
}