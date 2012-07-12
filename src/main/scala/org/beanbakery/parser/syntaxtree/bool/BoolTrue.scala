package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.Context

/**
 *
 */
object BoolTrue extends BoolExpr {
  def calculate(context: Context) = true
}