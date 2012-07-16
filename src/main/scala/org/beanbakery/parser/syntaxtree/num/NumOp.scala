package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext

/**
 *
 */
case class NumOp(a: NumExpr, op: Symbol, b: NumExpr) extends NumExpr {

  def calculate(context: BakeryContext): Double = {
    val leftVal = a.calculate(context)
    val rightVal = b.calculate(context)

    op match {
      case '+ => leftVal + rightVal
      case '- => leftVal - rightVal
      case '* => leftVal * rightVal
      case '/ => leftVal / rightVal
      case '^ => math.pow(leftVal, rightVal)
    }
  }

}