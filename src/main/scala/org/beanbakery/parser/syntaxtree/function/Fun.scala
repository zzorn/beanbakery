package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.Scope

/**
 * Function.
 */
trait Fun extends Expr {

  def paramDefs: List[ParamDef]

  def returnKind: Option[Kind]

  def getKind = null

  def evaluate(context: Scope): Any = {
    Closure(this, context)
  }

  def call(context: Scope): Any

  def doc: String

}