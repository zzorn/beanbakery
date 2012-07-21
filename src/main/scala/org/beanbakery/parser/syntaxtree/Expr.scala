package org.beanbakery.parser.syntaxtree

import kind.{Kind, SimpleKind}
import org.beanbakery.BakeryContext

/**
 *
 */
trait Expr {

  def getKind: Kind

  def calculate(context: BakeryContext): Any

}