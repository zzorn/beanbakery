package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.Scope

/**
 *
 */
case class ExpressionFun(paramDefs: List[ParamDef], returnKind: Option[Kind], expression: Expr) extends Fun {

  def call(context: Scope): Any = {
    expression.evaluate(context)
  }

}