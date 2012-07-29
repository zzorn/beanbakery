package org.beanbakery.parser.syntaxtree.bean

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.PathRef

/**
 *
 */
case class VarRef(id: PathRef) extends BeanExpr {

  def getKind = null

  def evaluate(context: Scope): Any = {
    //context.getVariable(id)
    null
    // TODO
  }

}