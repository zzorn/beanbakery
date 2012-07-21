package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.parser.syntaxtree.kind.BoolKind

/**
 *
 */
trait BoolExpr extends Expr {

  def calculate(context: BakeryContext): Boolean

  def getKind = BoolKind

}