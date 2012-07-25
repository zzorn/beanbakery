package org.beanbakery.parser.syntaxtree.propertyaccess

import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.Scope

/**
 *
 */
case class PropAccess(host: Expr, propertyName: Symbol) extends Expr {
  def getKind = null

  def calculate(context: Scope) = null
}