package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.Context

/**
 *
 */
case class NumOp(a: NumExpr, op: Symbol, b: NumExpr) extends NumExpr {

  def calculate(context: Context): Double = {
    val leftVal = a.calculate(context)
    val rightVal = b.calculate(context)

    op match {
      case '+ => leftVal + rightVal
      case '- => leftVal - rightVal
      case '* => leftVal * rightVal
      case '/ => leftVal / rightVal
    }
  }

}