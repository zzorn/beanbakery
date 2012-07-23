package org.beanbakery.parser

import syntaxtree.{Expr, Def}
import org.beanbakery.BakeryContext

/**
 *
 */
case class Block(definitions: List[Def], result: Expr) extends Expr {
  def getKind = null

  def calculate(context: BakeryContext) = null
}