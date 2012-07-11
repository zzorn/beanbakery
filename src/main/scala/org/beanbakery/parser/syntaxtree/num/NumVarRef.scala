package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.parser.Context

/**
 * Reference to variable with number value.
 */
case class NumVarRef(variableName: Symbol) extends NumExpr {
  def calculate(context: Context): Double = {
    context.getNumVariable(variableName)
  }
}