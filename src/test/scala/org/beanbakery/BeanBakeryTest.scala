package org.beanbakery

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException
import java.awt.{Point, Color}

/**
 *
 */
class BeanBakeryTest  extends FunSuite{

  test("Creating a bean") {
    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Point())
    bakery.addBeanClass('TestBean, classOf[TestBean])


    val posRecipe = new BeanRecipe('Point)
    posRecipe.setInitializer('x, Const(-100))
    posRecipe.setInitializer('y, Const(200))

    val recipe = new BeanRecipe('TestBean)
    recipe.setInitializer('pos, posRecipe)
    recipe.setInitializer('radius, Const(6.28))
    recipe.setInitializer('segments, Const(8))

    val bean: TestBean = recipe.calculateValue(bakery, new BakeryContext())

    assert(bean.pos === new Point(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)
  }

}




case class TestBean(var pos: Point = null, var radius: Double = 1, var segments: Int = 2)