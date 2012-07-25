package org.beanbakery.parser.syntaxtree

import org.beanbakery.Scope

/**
 *
 */
case class Parens(expr: Expr) extends Expr{
  def getKind = expr.getKind
  def calculate(context: Scope) = expr.calculate(context)
}