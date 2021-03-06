package org.beanbakery.parser.syntaxtree

import org.beanbakery.{DefaultScopeContent, Scope}

/**
 *
 */
case class Module(name: Symbol, imports: List[Import], definitions: List[Def]) {

  def evaluate(scope: Scope = DefaultScopeContent.defaultScope): Scope = {

    var currentScope = scope
    definitions foreach {d =>
      val name = d.id
      val expr = d.expr
      val value = expr.evaluate(currentScope)
      currentScope = currentScope.expand(Map(name -> value))
    }
    currentScope
  }

}