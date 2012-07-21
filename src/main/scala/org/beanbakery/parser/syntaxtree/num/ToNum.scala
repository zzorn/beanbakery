package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.BakeryContext

/**
 * Casts an expression to number.
 */
case class ToNum(expr: Expr) extends NumExpr {

  // TODO: Add some type checking.

  def calculate(context: BakeryContext) = expr.calculate(context).asInstanceOf[Double]
}