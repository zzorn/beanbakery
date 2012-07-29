package org.beanbakery.parser

import syntaxtree.{Expr, Def}
import org.beanbakery.Scope

/**
 *
 */
case class Block(definitions: List[Def], result: Expr) extends Expr {
  def getKind = null

  def evaluate(context: Scope) = null
}