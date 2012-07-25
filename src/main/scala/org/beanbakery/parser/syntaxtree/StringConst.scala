package org.beanbakery.parser.syntaxtree

import org.beanbakery.Scope

/**
 *
 */
case class StringConst(value: String) extends Expr {
  def getKind = null

  def calculate(context: Scope) = value
}