package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext

/**
 * Reference to variable with number value.
 */
case class NumVarRef(variableName: Symbol) extends NumExpr {
  def calculate(context: BakeryContext): Double = {
    context.getNumVariable(variableName)
  }
}