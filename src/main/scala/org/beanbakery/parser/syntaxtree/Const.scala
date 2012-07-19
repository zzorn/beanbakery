package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
case class Const(value: Any, exprType: ExprType) extends Expr {

  def calculate(context: BakeryContext) = value

  def getType = exprType
}