package org.beanbakery.parser.syntaxtree.bean

import org.beanbakery.parser.syntaxtree.bool.BoolExpr
import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 * If expression.
 */
case class IfExpr(expression: Expr, thenValue: Expr, elseValue: Expr) extends Expr {

  def getKind = null // TODO

  def evaluate(context: Scope): Any = {
    if (expression.evaluate(context).asInstanceOf[Boolean]) thenValue.evaluate(context)
    else elseValue.evaluate(context)
  }
}