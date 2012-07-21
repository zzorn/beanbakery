package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class BoolNot(a: Expr) extends BoolExpr {
  def calculate(context: BakeryContext): Boolean = {
    !a.calculate(context).asInstanceOf[Boolean]
  }
}