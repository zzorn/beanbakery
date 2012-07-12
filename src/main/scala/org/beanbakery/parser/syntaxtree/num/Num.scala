package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.Context

/**
 * Number constant
 */
case class Num(value: Double) extends NumExpr {
  def calculate(context: Context) = value
}