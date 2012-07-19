package org.beanbakery.parser.syntaxtree

/**
 *
 */
case class PropDef(id: Symbol, kind: Option[ExprType], expr: Expr) extends Statement {

}