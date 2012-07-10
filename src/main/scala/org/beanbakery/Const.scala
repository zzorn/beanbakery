package org.beanbakery

/**
 *
 */
case class Const(value: Any) extends PropertyInitializer {

  def calculateValue[T <: Any](bakery: BeanBakery, context: BakeryContext) = value.asInstanceOf[T]

}