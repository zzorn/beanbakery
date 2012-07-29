package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.Scope

/**
 *
 */
object BoolTrue extends BoolExpr {
  def evaluate(context: Scope) = true
}