package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext

/**
 *
 */
case class BoolOp(a: BoolExpr, op: Symbol, b: BoolExpr) extends BoolExpr {

  val allowedOps = Set('and, 'or, 'xor)

  def calculate(context: BakeryContext): Boolean = {
    val aVal = a.calculate(context)
    val bVal = b.calculate(context)

    op match {
      case 'and => aVal && bVal
      case 'or => aVal || bVal
      case 'xor => aVal != bVal
    }
  }

}