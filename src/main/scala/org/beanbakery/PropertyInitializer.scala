package org.beanbakery

/**
 * Calculates the value for a property.
 */
trait PropertyInitializer {

  def calculateValue[T <: Any](context: Scope): T

}