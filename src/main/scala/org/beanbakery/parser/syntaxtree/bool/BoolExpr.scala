package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.parser.syntaxtree.kind.BoolKind

/**
 *
 */
trait BoolExpr extends Expr {

  def evaluate(context: Scope): Boolean

  def getKind = BoolKind

}