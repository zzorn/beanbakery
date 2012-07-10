package org.beanbakery

import org.scalastuff.scalabeans.MutablePropertyDescriptor
import utils.ParameterChecker

/**
 * Information about how to create a bean and initialize its properties.
 */
case class BeanRecipe(var beanTypeId: Symbol,
                      var propertyInitializers: Map[Symbol, PropertyInitializer] = Map()
                       ) extends PropertyInitializer {
  ParameterChecker.requireIsIdentifier(beanTypeId, 'beanTypeId)


  def setInitializer(propertyId: Symbol, initializer: PropertyInitializer) {
    ParameterChecker.requireIsIdentifier(propertyId, 'propertyId)
    ParameterChecker.requireNotNull(initializer, 'initializer)

    propertyInitializers += propertyId -> initializer
  }


  def calculateValue[T <: Any](bakery: BeanBakery, context: BakeryContext): T = {
    ParameterChecker.requireNotNull(bakery, 'bakery)
    ParameterChecker.requireNotNull(context, 'context)

    // Create bean
    val bean = bakery.createBean(beanTypeId)

    // Initialize properties
    val descriptor = bakery.getDescriptor(bean.getClass)
    propertyInitializers foreach {
      entry =>
        val id = entry._1
        val initializer = entry._2

        descriptor.property(id.name) match {
          case Some(mutableProperty: MutablePropertyDescriptor) =>
            mutableProperty.set(bean, initializer.calculateValue(bakery, context))
          case Some(x) =>
            throw new BeanBakeryException("The property '" + id.name + "' on bean type '" + descriptor.name + "' is not mutable, can not initialize it!")
          case None =>
            throw new BeanBakeryException("No property named '" + id.name + "' was found on bean type '" + descriptor.name + "', can not initialize it!")
        }
    }

    bean.asInstanceOf[T]
  }

}