package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
trait NumExpr extends Expr {

  def calculate(context: BakeryContext): Double

  def getType = NumType

}