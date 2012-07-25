package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 * Invokes a function.
 */
case class FunCall(functionExpr: Expr, parameters: List[CallParam]) extends Expr {

  def getKind = null

  def calculate(context: Scope) = null

}