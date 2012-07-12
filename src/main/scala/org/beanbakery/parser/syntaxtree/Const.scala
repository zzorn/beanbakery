package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
case class Const(value: Any) extends Expr {

  def calculate(context: BakeryContext) = value

}