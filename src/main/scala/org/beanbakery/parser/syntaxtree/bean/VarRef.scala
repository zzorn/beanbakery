package org.beanbakery.parser.syntaxtree.bean

import org.beanbakery.Scope
import org.beanbakery.parser.syntaxtree.PathRef

/**
 *
 */
case class VarRef(ref: PathRef) extends BeanExpr {

  def getKind = null

  def evaluate(context: Scope): Any = {
    context.getValueAtPath(ref.path).getOrElse(throw new Error("Referenced variable named '"+ref+"' not found."))
  }

}