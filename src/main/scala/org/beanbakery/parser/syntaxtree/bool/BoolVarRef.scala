package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.parser.Context

/**
 * Reference to variable with boolean value.
 */
case class BoolVarRef(variableName: Symbol) extends BoolExpr {
  def calculate(context: Context): Boolean = {
    context.getBoolVariable(variableName)
  }
}