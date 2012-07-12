package org.beanbakery.parser.syntaxtree.bool

import org.beanbakery.BakeryContext

/**
 * Reference to variable with boolean value.
 */
case class BoolVarRef(variableName: Symbol) extends BoolExpr {
  def calculate(context: BakeryContext): Boolean = {
    context.getBoolVariable(variableName)
  }
}