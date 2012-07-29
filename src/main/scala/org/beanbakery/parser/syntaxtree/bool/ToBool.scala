package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.Scope

/**
 * Casts an expression to boolean.
 */
case class ToBool(expr: Expr) extends BoolExpr {

  // TODO: Add some type checking.

  def evaluate(context: Scope) = expr.evaluate(context).asInstanceOf[Boolean]
}