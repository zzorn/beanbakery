package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.Scope

/**
 * Function.
 */
case class Fun(paramDefs: List[ParamDef], returnKind: Option[Kind], expression: Expr) extends Expr {
  def getKind = null

  def calculate(context: Scope) = null
}