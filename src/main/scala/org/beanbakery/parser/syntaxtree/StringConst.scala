package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
case class StringConst(value: String) extends Expr {
  def getKind = null

  def calculate(context: BakeryContext) = value
}