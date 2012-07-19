package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
trait Expr {

  def getType: ExprType

  def calculate(context: BakeryContext): Any

}