package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
trait BoolExpr extends Expr {

  def calculate(context: BakeryContext): Boolean

}