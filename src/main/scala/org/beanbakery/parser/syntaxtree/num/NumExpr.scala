package org.beanbakery.parser.syntaxtree.num

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.Expr
import org.beanbakery.parser.syntaxtree.kind.NumKind

/**
 *
 */
trait NumExpr extends Expr {

  def calculate(context: BakeryContext): Double

  def getKind = NumKind

}