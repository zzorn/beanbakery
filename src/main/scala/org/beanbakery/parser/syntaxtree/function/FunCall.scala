package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.Expr

/**
 * Invokes a function.
 */
case class FunCall(functionExpr: Expr, parameters: List[CallParam]) extends Expr {

  def getKind = null

  def evaluate(context: Scope): Any  = {
    functionExpr.evaluate(context) match {
      case c: Closure =>
        // Evaluate parameters
        var params = Map[Symbol, Any]()
        var index = 0
        parameters foreach {p =>
          val id =
            if (p.id.isDefined) p.id.get
            else {
              if (index >= c.fun.paramDefs.size) throw new Error("Too many parameters given when invoking function, expected "+c.fun.paramDefs.size+", but got "+parameters.size+".")
              else {
                val indexName = c.fun.paramDefs(index).name
                index += 1
                indexName
              }
            }

          params += id -> p.expr.evaluate(context)
        }

        // Invoke function closure
        c.invoke(params)

      case x => throw new Error("Function value expected but '"+x+"' found when evaluating a call on " + functionExpr)
    }
  }

}