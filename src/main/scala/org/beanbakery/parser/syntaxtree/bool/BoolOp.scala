package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class BoolOp(a: Expr, op: Symbol, b: Expr) extends BoolExpr {

  val allowedOps = Set('and, 'or, 'xor)

  def calculate(context: Scope): Boolean = {
    val aVal = a.calculate(context).asInstanceOf[Boolean]
    val bVal = b.calculate(context).asInstanceOf[Boolean]

    op match {
      case 'and => aVal && bVal
      case 'or => aVal || bVal
      case 'xor => aVal != bVal
    }
  }

}