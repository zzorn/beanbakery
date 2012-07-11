package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.Context
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
trait BoolExpr extends Expr {

  def calculate(context: Context): Boolean

}