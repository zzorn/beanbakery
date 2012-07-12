package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext

/**
 *
 */
object BoolFalse extends BoolExpr {
  def calculate(context: BakeryContext) = false
}