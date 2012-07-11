package org.beanbakery.parser.syntaxtree

import org.beanbakery.parser.Context

/**
 *
 */
trait Expr {

  def calculate(context: Context): Any

}