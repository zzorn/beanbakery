package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext

/**
 * Negated number.
 */
case class NumNeg(a: NumExpr) extends NumExpr {
  def calculate(context: BakeryContext): Double = {
    -a.calculate(context)
  }
}