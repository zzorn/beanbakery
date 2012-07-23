package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
case class Parens(expr: Expr) extends Expr{
  def getKind = expr.getKind
  def calculate(context: BakeryContext) = expr.calculate(context)
}