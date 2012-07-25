package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class BoolNot(a: Expr) extends BoolExpr {
  def calculate(context: Scope): Boolean = {
    !a.calculate(context).asInstanceOf[Boolean]
  }
}