package org.beanbakery.parser.syntaxtree

import org.beanbakery.BakeryContext

/**
 *
 */
trait Expr {

  def calculate(context: BakeryContext): Any

}