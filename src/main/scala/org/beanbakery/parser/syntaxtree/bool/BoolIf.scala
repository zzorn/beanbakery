package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext


/**
 * If expression with boolean result.
 */
case class BoolIf(expression: BoolExpr, thenValue: BoolExpr, elseValue: BoolExpr) extends BoolExpr {
  def calculate(context: BakeryContext): Boolean = {
    if (expression.calculate(context)) {
      thenValue.calculate(context)
    }
    else {
      elseValue.calculate(context)
    }
  }
}