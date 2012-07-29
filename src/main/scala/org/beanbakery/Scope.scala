package org.beanbakery

import org.scalastuff.scalabeans.BeanDescriptor
import parser.syntaxtree.function.Closure
import parser.syntaxtree.PathRef

/**
 *
 */
case class Scope(values: Map[Symbol, Any] = Map(),
                 subScopes: Map[Symbol, Scope] = Map(),
                 parent: Scope = null) {

  def getValueAtPath(path: List[Symbol]): Option[Any] = {
    if (path.isEmpty) None // Empty path
    else if (path.tail.isEmpty) {
      // Single element path
      if (hasValue(path.head)) Some(getValue(path.head))
      else None
    }
    else {
      // Multi element path
      // Try to find in local subscopes
      val subscopeValue: Option[Any] = if (subScopes.contains(path.head)) subScopes(path.head).getValueAtPath(path.tail) else None

      if (subscopeValue == None) {
        // Try to delegate to parent
        if (parent != null) parent.getValueAtPath(path)
        else None
      }
      else subscopeValue
    }
  }

  def getValue(id: Symbol): Any = values.getOrElse(id, if (parent != null) parent.getValue(id) else throw new Error("Value '"+id.name+"' not found in scope."))
  def getBool(id: Symbol): Boolean = getValue(id).asInstanceOf[Boolean]
  def getNum(id: Symbol): Double = getValue(id).asInstanceOf[Double]

  def callFun(funName: Symbol, namedParams: Map[Symbol, Any] = Map()): Any = {
    getValue(funName).asInstanceOf[Closure].invoke(namedParams)
  }

  def hasValue(id: Symbol): Boolean = values.contains(id) || (if (parent != null) parent.hasValue(id) else false)

  def expand(additionalValues: Map[Symbol, Any] = Map(), additionalSubScopes: Map[Symbol, Scope] = Map()): Scope = {
    Scope(additionalValues, additionalSubScopes, this)
  }

}