package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.BakeryContext

/**
 * Casts an expression to boolean.
 */
case class ToBool(expr: Expr) extends BoolExpr {

  // TODO: Add some type checking.

  def calculate(context: BakeryContext) = expr.calculate(context).asInstanceOf[Boolean]
}