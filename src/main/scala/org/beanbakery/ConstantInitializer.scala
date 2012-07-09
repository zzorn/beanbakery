package org.beanbakery

/**
 *
 */
case class ConstantInitializer(value: AnyRef) extends PropertyInitializer {

  def calculateValue[T <: AnyRef](bakery: BeanBakery, context: BakeryContext) = value.asInstanceOf[T]

}