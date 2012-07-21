package org.beanbakery

import parser.syntaxtree.Expr

/**
 *
 */
class DynamicBean(base: Bean) extends Bean {

  private var props: Map[Symbol, Any] = Map()
  private var initializers: List[(Symbol, Expr)] = Nil


  def createProperty(name: Symbol, expr: Expr) {
  }

  def set(name: Symbol, value: Any) {
    if (props.contains(name)) props += name -> value
    else throw new Error("Unknown property '"+name.name+"'")
  }

  def get(name: Symbol) = props(name)

  def properties = props

  def invoke(params: Map[Symbol, Any]): Any = {
    val newBean = new DynamicBean(this)


  }


}