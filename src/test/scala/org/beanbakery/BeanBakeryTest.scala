package org.beanbakery

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException
import java.awt.{Point, Color}
import parser.ExpressionParser
import parser.syntaxtree.Const
import java.awt.geom.Point2D

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
    posRecipe.setExpression('x, parser.parseString("-100", bakery.createContext()))
    posRecipe.setExpression('y, parser.parseString(" 10*10 + 10  *  10  ", bakery.createContext()))

    val recipe = new BeanRecipe('TestBean)
    recipe.setExpression('pos, posRecipe)
    recipe.setExpression('radius, parser.parseString("3.14 * 2", bakery.createContext()))
    recipe.setExpression('segments, parser.parseString("2 * 2 + (if 1 > 0 then 2 else 0) ^ 2", bakery.createContext()))

    val bean: TestBean = recipe.calculate(new BakeryContext(bakery)).asInstanceOf[TestBean]

    assert(bean.pos === new Pos(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)

  }

  // TODO: More tests for expression parser.

}


case class Pos(var x : Double = 0, var y : Double = 0)

case class TestBean(var pos: Pos = null, var radius: Double = 1, var segments: Double = 2)