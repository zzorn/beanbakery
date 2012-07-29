package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.parser.syntaxtree.kind.NumKind

/**
 *
 */
trait NumExpr extends Expr {

  def evaluate(context: Scope): Double

  def getKind = NumKind

}