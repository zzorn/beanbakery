package org.beanbakery

/**
 * Indicates some error when creating beans.
 */
case class BeanBakeryException(message: String, cause: Throwable = null) extends Error(message, cause) {

}