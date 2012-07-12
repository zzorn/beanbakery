package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.syntaxtree.bool.BoolExpr
import org.beanbakery.BakeryContext

/**
 * If expression with numerical result.
 */
case class NumIf(expression: BoolExpr, thenValue: NumExpr, elseValue: NumExpr) extends NumExpr {
  def calculate(context: BakeryContext): Double = {
    if (expression.calculate(context)) {
      thenValue.calculate(context)
    }
    else {
      elseValue.calculate(context)
    }
  }
}