package org.beanbakery

/**
 * Calculates the value for a property.
 */
trait PropertyInitializer {

  def calculateValue[T <: AnyRef](bakery: BeanBakery, context: BakeryContext): T

}