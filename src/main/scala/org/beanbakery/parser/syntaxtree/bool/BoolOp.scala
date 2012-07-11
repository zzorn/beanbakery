package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.Context

/**
 *
 */
case class BoolOp(a: BoolExpr, op: Symbol, b: BoolExpr) extends BoolExpr {

  val allowedOps = Set('and, 'or, 'xor)

  def calculate(context: Context): Boolean = {
    val aVal = a.calculate(context)
    val bVal = b.calculate(context)

    op match {
      case 'and => aVal && bVal
      case 'or => aVal || bVal
      case 'xor => aVal != bVal
    }
  }

}