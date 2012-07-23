package org.beanbakery

import org.scalastuff.scalabeans.BeanDescriptor
import org.scalastuff.scalabeans.Preamble._
import parser.ExpressionParser
import parser.syntaxtree.{Module, Block, Expr}
import utils.ParameterChecker

/**
 * Main handle for the BeanBakery library.
 * To use, create a new instance, then register the classes that BeanRecipes are allowed to create instances of,
 * then load or construct a BeanRecipe, provide it with any context variables, and create instances of initialized beans with it.
 */
class BeanBakery {

  // Cache BeanDescriptors so that we do not have to create new ones each time.
  private var descriptors: Map[Class[_ <: AnyRef], BeanDescriptor] = Map()

  private var beanFactories: List[(Symbol) => Option[AnyRef]] = Nil

  private val parser = new ExpressionParser()

  def parseDocument(document: String): Doc = {
    parser.parseDocumentString(document)
  }


  def createContext(includeDefaultFunctions: Boolean = true): BakeryContext = {
    new BakeryContext(this, includeDefaultFunctions)
  }

  def addBeanFactory(factory: (Symbol) => Option[AnyRef]) {
    ParameterChecker.requireNotNull(factory, 'factory)

    beanFactories ::= factory
  }

  def addBeanCreator(beanCreator: BeanCreator) {
    ParameterChecker.requireNotNull(beanCreator, 'beanCreator)

    beanFactories ::= beanCreator
  }

  def addBeanCreator(id: Symbol, creator: () => AnyRef) {
    ParameterChecker.requireNotNull(creator, 'creator)

    beanFactories ::= BeanCreator(id, () => creator())
  }

  def addBeanClass(id: Symbol, beanClass: Class[_ <: AnyRef]) {
    ParameterChecker.requireIsIdentifier(id, 'id)
    ParameterChecker.requireNotNull(beanClass, 'beanClass)

    beanFactories ::= BeanCreator(id, () => getDescriptor(beanClass).newInstance())
  }

  def addBeanClass(beanClass: Class[_ <: AnyRef]) {
    ParameterChecker.requireNotNull(beanClass, 'beanClass)

    addBeanClass(Symbol(beanClass.getSimpleName), beanClass)
  }


  def getDescriptor(beanClass: Class[_ <: AnyRef]): BeanDescriptor = {
    ParameterChecker.requireNotNull(beanClass, 'beanClass)

    var descriptor = descriptors.getOrElse(beanClass, null)

    if (descriptor == null) {
      descriptor = descriptorOf(beanClass)
      descriptors += beanClass -> descriptor
    }

    descriptor
  }

  def createBean(id: Symbol): AnyRef = {
    ParameterChecker.requireIsIdentifier(id, 'id)

    beanFactories foreach {
      beanFactory =>
        val bean = beanFactory(id)
        if (bean != None) return bean.get
    }

    throw new BeanBakeryException("Could not create bean with name '" + id.name + "', no BeanFactory found.")
  }

}