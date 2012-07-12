package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.ExprConstants._
import org.beanbakery.parser.syntaxtree.num.NumExpr

/**
 * Supports comparison of up to three numbers with >, <, >=, and <= operators.
 */
case class ComparisonOp(a: NumExpr,
                        op1: Symbol,
                        b: NumExpr,
                        op2: Symbol = null,
                        c: NumExpr = null) extends BoolExpr {

  def calculate(context: BakeryContext): Boolean = {
    val aVal = a.calculate(context)
    val bVal = b.calculate(context)

    if (op2 != null) {
      val cVal = c.calculate(context)
      compare(aVal, op1, bVal) && compare(bVal, op2, cVal)
    }
    else {
      compare(aVal, op1, bVal)
    }
  }

  private def compare(v1: Double, op: Symbol, v2: Double): Boolean = {
    // Use epsilon to compensate for double calculation inaccuracies.
    op match {
      case '> => v1 > v2 + Epsilon
      case '>= => v1 >= v2 - Epsilon
      case '< => v1 < v2 - Epsilon
      case '<= => v1 <= v2 + Epsilon
    }
  }
}