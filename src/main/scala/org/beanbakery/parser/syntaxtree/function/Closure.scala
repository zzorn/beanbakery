package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 * A function with a scope.
 */
case class Closure(fun: Fun, scope: Scope) {

  def invoke[T](parameters: Map[Symbol, Any]): T = {

    parameters foreach {e =>
      val name = e._1
      val value = e._2

      // Check that no provided parameter is unknown
      val paramDef = fun.paramDefs.find(pd => pd.name == name)
      if (paramDef.isEmpty) throw new Error("Unknown parameter with name '"+name.name+"' and value '"+value+"'")

      // Check parameter type
      // TODO
    }

    // If a parameter is not provided, use default value, if a default value exists, else throw error
    var defaultParams = Map[Symbol, Any]()
    fun.paramDefs foreach {pd =>
      if (!parameters.contains(pd.name)) {
        pd.default match {
          case Some(defaultExpr: Expr) => defaultParams += pd.name -> defaultExpr.evaluate(scope)
          case None => throw new Error("Parameter '"+pd.name.name+"' missing.")
        }
      }
    }

    // Call function with original function scope + parameters in scope
    fun.call(scope.expand(parameters ++ defaultParams)).asInstanceOf[T]
  }

}