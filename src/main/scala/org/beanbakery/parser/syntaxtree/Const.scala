package org.beanbakery.parser.syntaxtree

import kind.SimpleKind
import org.beanbakery.Scope

/**
 *
 */
case class Const(value: Any, exprType: SimpleKind) extends Expr {

  def evaluate(context: Scope) = value

  def getKind = exprType
}