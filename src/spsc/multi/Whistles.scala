package spsc.multi

import spsc._

trait Whistle {
  def name: String
  def accept(t: Tree, n: Node): Boolean
}

object SimpleWhistle extends Whistle {
  val name = "size < 9"
  def accept(t: Tree, n: Node) = n.expr match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => x.size < 9
  }
}

object HEWhistle extends Whistle {
  val name = "homeomorphic embedding"
  def accept(t: Tree, n: Node) = n.expr match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => n.ancestors.forall { a =>
      a.expr match {
        case Let(_, _) => true
        case _ => !HE.he(a.expr, x)
      }
    }
  }
}

object HEWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding with redex"
  def accept(t: Tree, n: Node) = n.expr match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => n.ancestors.forall { a =>
      a.expr match {
        case Let(_, _) => true
        case _ => !HE.he_*(a.expr, x)
      }
    }
  }
}


object HEByCouplingWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling"
  def accept(t: Tree, n: Node) = n.expr match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => n.ancestors.forall { a =>
      a.expr match {
        case Let(_, _) => true
        case _ => !HE.heByCoupling(a.expr, x)
      }
    }
  }
}

object HEByCouplingWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling with redex"
  def accept(t: Tree, n: Node) = n.expr match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => n.ancestors.forall { a =>
      a.expr match {
        case Let(_, _) => true
        case _ => !(HE.heByCoupling(a.expr, x) && HE.b(a.expr) == HE.b(x))
      }
    }
  }
}