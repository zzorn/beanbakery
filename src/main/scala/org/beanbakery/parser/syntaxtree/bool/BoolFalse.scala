package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.Context

/**
 *
 */
object BoolFalse extends BoolExpr {
  def calculate(context: Context) = false
}