package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class NumOp(a: Expr, op: Symbol, b: Expr) extends NumExpr {

  def calculate(context: BakeryContext): Double = {
    val leftVal = a.calculate(context).asInstanceOf[Double]
    val rightVal = b.calculate(context).asInstanceOf[Double]

    op match {
      case '+ => leftVal + rightVal
      case '- => leftVal - rightVal
      case '* => leftVal * rightVal
      case '/ => leftVal / rightVal
      case '^ => math.pow(leftVal, rightVal)
    }
  }

}