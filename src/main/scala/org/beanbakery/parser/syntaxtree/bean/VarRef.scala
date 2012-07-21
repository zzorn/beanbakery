package org.beanbakery.parser.syntaxtree.bean

import org.beanbakery.BakeryContext

/**
 *
 */
case class VarRef(id: Symbol) extends BeanExpr {

  def getKind = null

  def calculate(context: BakeryContext): Any = {
    context.getVariable(id)
  }

}