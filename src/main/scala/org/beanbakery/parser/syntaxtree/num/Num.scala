package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.Scope

/**
 * Number constant
 */
case class Num(value: Double) extends NumExpr {
  def calculate(context: Scope) = value
}