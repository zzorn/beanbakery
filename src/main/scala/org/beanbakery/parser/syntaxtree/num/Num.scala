package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext

/**
 * Number constant
 */
case class Num(value: Double) extends NumExpr {
  def calculate(context: BakeryContext) = value
}