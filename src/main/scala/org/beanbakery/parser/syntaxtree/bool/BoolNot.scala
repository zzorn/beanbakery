package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext

/**
 *
 */
case class BoolNot(a: BoolExpr) extends BoolExpr {
  def calculate(context: BakeryContext): Boolean = {
    !a.calculate(context)
  }
}