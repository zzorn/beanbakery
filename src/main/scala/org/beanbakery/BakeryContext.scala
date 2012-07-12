package org.beanbakery

import org.scalastuff.scalabeans.BeanDescriptor

/**
 *
 */
class BakeryContext(val bakery: BeanBakery) {

  private var _variables: Map[Symbol, Any] = Map()

  private var _numFunctions1: Map[Symbol, (Double) => Double] = Map()
  private var _numFunctions2: Map[Symbol, (Double, Double) => Double] = Map()
  private var _numFunctions3: Map[Symbol, (Double, Double, Double) => Double] = Map()
  private var _numFunctions4: Map[Symbol, (Double, Double, Double, Double) => Double] = Map()
  private var _numFunctions5: Map[Symbol, (Double, Double, Double, Double, Double) => Double] = Map()

  def getDescriptor(beanClass: Class[_ <: AnyRef]): BeanDescriptor = bakery.getDescriptor(beanClass)

  def createBean(id: Symbol): AnyRef = bakery.createBean(id)

  def getVariable(id: Symbol): Any = _variables(id)

  def getBoolVariable(id: Symbol): Boolean = _variables(id).asInstanceOf[Boolean]

  def getNumVariable(id: Symbol): Double = _variables(id).asInstanceOf[Double]

  def setVariable(id: Symbol, value: Any) {
    _variables += id -> value
  }

  def addFunction(id: Symbol)(fun: Any) {
    fun match {
      case f: ((Double) => Double) => _numFunctions1 += id -> f
      case f: ((Double, Double) => Double) => _numFunctions2 += id -> f
      case f: ((Double, Double, Double) => Double) => _numFunctions3 += id -> f
      case f: ((Double, Double, Double, Double) => Double) => _numFunctions4 += id -> f
      case f: ((Double, Double, Double, Double, Double) => Double) => _numFunctions5 += id -> f
    }
  }

  def hasVariable(id: Symbol): Boolean = _variables.contains(id)

  def getNumFunction1(id: Symbol): (Double) => Double = _numFunctions1(id)

  def getNumFunction2(id: Symbol): (Double, Double) => Double = _numFunctions2(id)

  def getNumFunction3(id: Symbol): (Double, Double, Double) => Double = _numFunctions3(id)

  def getNumFunction4(id: Symbol): (Double, Double, Double, Double) => Double = _numFunctions4(id)

  def getNumFunction5(id: Symbol): (Double, Double, Double, Double, Double) => Double = _numFunctions5(id)


}