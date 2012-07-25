package org.beanbakery.parser.syntaxtree

import kind.{Kind, SimpleKind}
import org.beanbakery.Scope

/**
 *
 */
trait Expr {

  def getKind: Kind

  def calculate(context: Scope): Any

}