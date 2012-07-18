package org.beanbakery

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException
import java.awt.{Point, Color}
import parser.ExpressionParser
import parser.syntaxtree.Const
import java.awt.geom.Point2D
import scala.math._

/**
 *
 */
class BeanBakeryTest  extends FunSuite{

  test("Creating a bean") {
    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Pos())
    bakery.addBeanClass('TestBean, classOf[TestBean])


    val posRecipe = new BeanRecipe('Point)
    posRecipe.setExpression('x, Const(-100))
    posRecipe.setExpression('y, Const(200))

    val recipe = new BeanRecipe('TestBean)
    recipe.setExpression('pos, posRecipe)
    recipe.setExpression('radius, Const(6.28))
    recipe.setExpression('segments, Const(8))

    val bean: TestBean = recipe.calculate(new BakeryContext(bakery)).asInstanceOf[TestBean]

    assert(bean.pos === new Pos(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)
  }

  test("Parse expression") {
    val parser = new ExpressionParser()

    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Pos())
    bakery.addBeanClass('TestBean, classOf[TestBean])

    val posRecipe = new BeanRecipe('Point)
    posRecipe.setExpression('x, "-100", parser)
    posRecipe.setExpression('y, "10*10 + pow(10, 2)", parser)

    val recipe = new BeanRecipe('TestBean)
    recipe.setExpression('pos, posRecipe)
    recipe.setExpression('radius, "3.14 * a", parser)
    recipe.setExpression('segments, "2 * 2 + (if 1 > 0 then 2 else 0) ^ 2", parser)

    val context = new BakeryContext(bakery)
    context.setVariable('a, 2.0)
    context.addFunction('pow, pow(_, _))
    val bean: TestBean = recipe.calculate(context).asInstanceOf[TestBean]

    assert(bean.pos === new Pos(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)

  }

  // TODO: More tests for expression parser.

}


case class Pos(var x : Double = 0, var y : Double = 0)

case class TestBean(var pos: Pos = null, var radius: Double = 1, var segments: Double = 2)