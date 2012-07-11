package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.Context

/**
 *
 */
case class Not(a: BoolExpr) extends BoolExpr {
  def calculate(context: Context): Boolean = {
    !a.calculate(context)
  }
}