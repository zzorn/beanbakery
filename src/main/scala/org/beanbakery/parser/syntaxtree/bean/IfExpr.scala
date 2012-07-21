package org.beanbakery.parser.syntaxtree.bean

import org.beanbakery.parser.syntaxtree.bool.BoolExpr
import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr

/**
 * If expression.
 */
case class IfExpr(expression: Expr, thenValue: Expr, elseValue: Expr) extends Expr {

  def getKind = null // TODO

  def calculate(context: BakeryContext): Any = {
    if (expression.calculate(context).asInstanceOf[Boolean]) thenValue.calculate(context)
    else elseValue.calculate(context)
  }
}