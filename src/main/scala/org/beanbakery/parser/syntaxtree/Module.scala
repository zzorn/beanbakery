package org.beanbakery.parser.syntaxtree

import function.Fun
import org.beanbakery.Scope

/**
 *
 */
case class Module(name: Symbol, imports: List[Import], definitions: List[Def]) {

  def getVal(name: Symbol, scope: Scope) {
    // TODO
  }

}