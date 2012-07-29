package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class BoolNot(a: Expr) extends BoolExpr {
  def evaluate(context: Scope): Boolean = {
    !a.evaluate(context).asInstanceOf[Boolean]
  }
}