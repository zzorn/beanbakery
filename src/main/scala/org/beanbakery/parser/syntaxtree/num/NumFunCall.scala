package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext

/**
 * Function call with number result.
 */
case class NumFunCall(functionName: Symbol, params: List[NumExpr]) extends NumExpr {
  def calculate(context: BakeryContext): Double = {
    val ps = params map {p => p.calculate(context)}

    ps.size match {
      case 1 => context.getNumFunction1(functionName)(ps(0))
      case 2 => context.getNumFunction2(functionName)(ps(0), ps(1))
      case 3 => context.getNumFunction3(functionName)(ps(0), ps(1), ps(2))
      case 4 => context.getNumFunction4(functionName)(ps(0), ps(1), ps(2), ps(3))
      case 5 => context.getNumFunction5(functionName)(ps(0), ps(1), ps(2), ps(3), ps(4))
      case _ => throw new IllegalStateException("Unsupported number of parameters: " + ps.size)
    }
  }
}