package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 * Negated number.
 */
case class NumNeg(a: Expr) extends NumExpr {
  def evaluate(context: Scope): Double = {
    -a.evaluate(context).asInstanceOf[Double]
  }
}