package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr

/**
 * Negated number.
 */
case class NumNeg(a: Expr) extends NumExpr {
  def calculate(context: BakeryContext): Double = {
    -a.calculate(context).asInstanceOf[Double]
  }
}