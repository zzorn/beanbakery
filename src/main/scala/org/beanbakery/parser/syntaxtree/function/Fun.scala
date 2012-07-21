package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.BakeryContext

/**
 * Function.
 */
case class Fun(parametDefs: List[ParamDef], returnKind: Kind, expression: Expr) extends Expr {
  def getKind = null

  def calculate(context: BakeryContext) = null
}