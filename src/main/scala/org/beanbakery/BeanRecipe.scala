package org.beanbakery

import org.scalastuff.scalabeans.MutablePropertyDescriptor
import parser.ExpressionParser
import parser.syntaxtree.kind.{Kind, SimpleKind}
import parser.syntaxtree.Expr
import utils.ParameterChecker

/**
 * Information about how to create a bean and initialize its properties.
 */
case class BeanRecipe(exprType: SimpleKind,
                      var propertyExpressions: Map[Symbol, Expr] = Map()
                       ) extends Expr {

  def getKind = exprType

  def setExpression(propertyId: Symbol, expression: Expr) {
    ParameterChecker.requireIsIdentifier(propertyId, 'propertyId)
    ParameterChecker.requireNotNull(expression, 'expression)

    propertyExpressions += propertyId -> expression
  }

  def setExpression(propertyId: Symbol, expression: String, parser: ExpressionParser) {
    ParameterChecker.requireIsIdentifier(propertyId, 'propertyId)
    ParameterChecker.requireNotNull(expression, 'expression)

    propertyExpressions += propertyId -> parser.parseString(expression)
  }


  def calculate(context: BakeryContext): Any = {
    ParameterChecker.requireNotNull(context, 'context)

    // Create bean
    val bean = context.createBean(exprType.id)

    // Initialize properties
    val descriptor = context.getDescriptor(bean.getClass)
    propertyExpressions foreach {
      entry =>
        val id = entry._1
        val initializer = entry._2

        descriptor.property(id.name) match {
          case Some(mutableProperty: MutablePropertyDescriptor) =>
            mutableProperty.set(bean, initializer.calculate(context))
          case Some(x) =>
            throw new BeanBakeryException("The property '" + id.name + "' on bean type '" + descriptor.name + "' is not mutable, can not initialize it!")
          case None =>
            throw new BeanBakeryException("No property named '" + id.name + "' was found on bean type '" + descriptor.name + "', can not initialize it!")
        }
    }

    bean
  }

}