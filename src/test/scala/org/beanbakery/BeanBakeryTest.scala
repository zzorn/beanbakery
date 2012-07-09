package org.beanbakery

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException
import java.awt.Color

/**
 *
 */
class BeanBakeryTest  extends FunSuite{

  test("Creating a bean") {
    val bakery = new BeanBakery()
    bakery.addBeanClass('Color, classOf[Color])
    bakery.addBeanClass('Int, classOf[java.lang.Integer])
    bakery.addBeanClass(classOf[java.lang.Double])
    bakery.addBeanClass('TestBean, classOf[TestBean])


    val recipe = new BeanRecipe('TestBean)
    recipe.setInitializer('color, ConstantInitializer(Color.RED))
    recipe.setInitializer('radius, ConstantInitializer(java.lang.Double.valueOf(6.28)))
    recipe.setInitializer('segments, ConstantInitializer(java.lang.Integer.valueOf(3)))


    val bean: TestBean = recipe.calculateValue(bakery, new BakeryContext())

    assert(bean.color === Color.RED)
    assert(bean.radius === 6.28)
    assert(bean.segments === 3)
  }

}




case class TestBean(var color: Color = null, var radius: Double = 1, var segments: Int = 2)