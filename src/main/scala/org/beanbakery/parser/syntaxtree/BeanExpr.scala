package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
case class BeanExpr(baseType: Option[ExprType], params: Option[List[NamedParam]], block: Option[BlockDef]) extends Expr {

  def getType = baseType.getOrElse(BeanType)

  def calculate(context: BakeryContext): Any = {
    // TODO
  }

}