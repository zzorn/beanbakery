package org.beanbakery.parser.syntaxtree

import org.beanbakery.Scope

/**
 *
 */
case class Parens(expr: Expr) extends Expr{
  def getKind = expr.getKind
  def evaluate(context: Scope) = expr.evaluate(context)
}