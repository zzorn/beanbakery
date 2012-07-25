package org.beanbakery

import org.scalastuff.scalabeans.BeanDescriptor

/**
 *
 */
class Scope(val bakery: BeanBakery, includeDefaults: Boolean = false) {

  private var _variables: Map[Symbol, Any] = Map()

  private var _defs: Map[Symbol, Any] = Map()



  private var _numFunctions1: Map[Symbol, (Double) => Double] = Map()
  private var _numFunctions2: Map[Symbol, (Double, Double) => Double] = Map()
  private var _numFunctions3: Map[Symbol, (Double, Double, Double) => Double] = Map()
  private var _numFunctions4: Map[Symbol, (Double, Double, Double, Double) => Double] = Map()
  private var _numFunctions5: Map[Symbol, (Double, Double, Double, Double, Double) => Double] = Map()

  if (includeDefaults) DefaultContextContent.addToContext(this)

  def getDescriptor(beanClass: Class[_ <: AnyRef]): BeanDescriptor = bakery.getDescriptor(beanClass)

  def createBean(id: Symbol): AnyRef = bakery.createBean(id)

  def getVariable(id: Symbol): Any = _variables(id)

  def getBoolVariable(id: Symbol): Boolean = _variables(id).asInstanceOf[Boolean]

  def getNumVariable(id: Symbol): Double = _variables(id).asInstanceOf[Double]

  def setVariable(id: Symbol, value: Any) {
    _variables += id -> value
  }


  def addFunction(id: Symbol, fun: Double => Double) {
    _numFunctions1 += id -> fun
  }

  def addFunction(id: Symbol, fun: (Double, Double) => Double) {
    _numFunctions2 += id -> fun
  }

  def addFunction(id: Symbol, fun: (Double, Double, Double) => Double) {
    _numFunctions3 += id -> fun
  }

  def addFunction(id: Symbol, fun: (Double, Double, Double, Double) => Double) {
    _numFunctions4 += id -> fun
  }

  def addFunction(id: Symbol, fun: (Double, Double, Double, Double, Double) => Double) {
    _numFunctions5 += id -> fun
  }


  def hasVariable(id: Symbol): Boolean = _variables.contains(id)

  def getNumFunction1(id: Symbol): (Double) => Double = _numFunctions1(id)

  def getNumFunction2(id: Symbol): (Double, Double) => Double = _numFunctions2(id)

  def getNumFunction3(id: Symbol): (Double, Double, Double) => Double = _numFunctions3(id)

  def getNumFunction4(id: Symbol): (Double, Double, Double, Double) => Double = _numFunctions4(id)

  def getNumFunction5(id: Symbol): (Double, Double, Double, Double, Double) => Double = _numFunctions5(id)


}