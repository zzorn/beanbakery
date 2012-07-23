package org.beanbakery.parser.syntaxtree.bean

import org.beanbakery.BakeryContext
import org.beanbakery.parser.syntaxtree.PathRef

/**
 *
 */
case class VarRef(id: PathRef) extends BeanExpr {

  def getKind = null

  def calculate(context: BakeryContext): Any = {
    //context.getVariable(id)
    null
    // TODO
  }

}