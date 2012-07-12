package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext

/**
 *
 */
object BoolTrue extends BoolExpr {
  def calculate(context: BakeryContext) = true
}