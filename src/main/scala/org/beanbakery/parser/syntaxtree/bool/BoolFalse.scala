package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.Scope

/**
 *
 */
object BoolFalse extends BoolExpr {
  def evaluate(context: Scope) = false
}