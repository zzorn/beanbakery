package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class NumOp(a: Expr, op: Symbol, b: Expr) extends NumExpr {

  def evaluate(context: Scope): Double = {
    val leftVal = a.evaluate(context).asInstanceOf[Double]
    val rightVal = b.evaluate(context).asInstanceOf[Double]

    op match {
      case '+ => leftVal + rightVal
      case '- => leftVal - rightVal
      case '* => leftVal * rightVal
      case '/ => leftVal / rightVal
      case '^ => math.pow(leftVal, rightVal)
    }
  }

}