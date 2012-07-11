package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.Context

/**
 * Negated number.
 */
case class NumNeg(a: NumExpr) extends NumExpr {
  def calculate(context: Context): Double = {
    -a.calculate(context)
  }
}