package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.ExprConstants._
import org.beanbakery.parser.syntaxtree.Expr

/**
 * Operator for == and !=.
 */
case class EqualityComparisonOp(a: Expr, op: Symbol, b: Expr) extends BoolExpr {

  def calculate(context: BakeryContext): Boolean = {
    val v1 = a.calculate(context)
    val v2 = b.calculate(context)

    op match {
      case '== => compare(v1, v2)
      case '!= => !compare(v1, v2)
    }
  }


  private def compare(v1: Any, v2: Any): Boolean = {
    if (v1.isInstanceOf[Double] &&
      v2.isInstanceOf[Double]) {

      // For numbers, perform equality matching using epsilon, to account for drift-off and rounding errors in double calculations
      val n1 = v1.asInstanceOf[Double]
      val n2 = v2.asInstanceOf[Double]

      n2 - Epsilon < n1 && n1 < n2 + Epsilon
    }
    else {
      v1 == v2
    }
  }
}