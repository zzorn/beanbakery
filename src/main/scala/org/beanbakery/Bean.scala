package org.beanbakery

import parser.syntaxtree.Expr

/**
 *
 */
trait Bean {

  def invoke(params: Map[Symbol, Any]): Any

  def createProperty(name: Symbol, expr: Expr)
  def set(name: Symbol, value: Any)
  def get(name: Symbol): Any
  def properties: Map[Symbol, Any]

}