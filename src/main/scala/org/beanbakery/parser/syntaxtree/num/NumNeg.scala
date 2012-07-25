package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 * Negated number.
 */
case class NumNeg(a: Expr) extends NumExpr {
  def calculate(context: Scope): Double = {
    -a.calculate(context).asInstanceOf[Double]
  }
}