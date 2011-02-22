package spsc.multi

import spsc._

object Whistles {

  val simple = (t: Tree, n: Node) => n.expr.size < 7

  val he = (t: Tree, n: Node) => n.ancestors.forall(a => !HE.he_*(a.expr, n.expr))

}