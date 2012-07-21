package org.beanbakery.parser.syntaxtree.propertyaccess

import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.BakeryContext

/**
 *
 */
case class PropAccess(host: Expr, propertyName: Symbol) extends Expr {
  def getKind = null

  def calculate(context: BakeryContext) = null
}