package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.Context
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
trait NumExpr extends Expr {

  def calculate(context: Context): Double

}