package org.beanbakery

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import parser.ExpressionParser
import parser.syntaxtree.Const
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
    posRecipe.setExpression('y, "pow(10, 2) + testFun(5*2, 2)", parser)

    val recipe = new BeanRecipe('TestBean)
    recipe.setExpression('pos, posRecipe)
    recipe.setExpression('radius, "3.14 * a", parser)
    recipe.setExpression('segments, "2 * 2 + (if 1 > 0 then 2 else 0) ^ 2", parser)

    val context = new BakeryContext(bakery, includeDefaults = true)
    context.setVariable('a, 2.0)
    context.addFunction('testFun, pow(_, _))
    val bean: TestBean = recipe.calculate(context).asInstanceOf[TestBean]

    assert(bean.pos === new Pos(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)

  }


  test("Parsing whole bean from text") {

    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Pos())
    bakery.addBeanClass('TestBean, classOf[TestBean])


    bakery.parseString(
      """
        |
        |// Create a new instance of a registered bean type
        |rootPos := Pos (
        |  // Statements either separated by newline, or comma if on the same line
        |  x = 4 + 5
        |  y = sqrt(10)
        |)
        |
        |
        |
        |// If no base class specified, a new empty dynamic bean is created
        |// Beans can be used both as instances, or archetypes when creating new beans.
        |Flower := {
        |
        |  // The := operation defines new property.
        |  // Specifying type is optional, if not specified, type of default value used.
        |  // The default value is required.
        |  // Trying to assign a value to a non-existing property with = produces an error.
        |  hue := 0.5
        |  sat := 0.2
        |  radius : Num := 10
        |}
        |
        |
        |// Extending an existing bean to create a new dynamic bean is done with a block {} after the typename, an base instance, or a constructor call.
        |TestStructure := TestBean (segments = 4) {
        |
        |  size : Num := 5
        |
        |  radius = pow(10, size)
        |
        |  tipStructure : Flower = Flower(hue = 0.1)
        |
        |  pos = Pos {
        |    x = 10
        |    y = size * 5
        |  }
        |
        |  tipPos : Pos = Pos{x = pos.x + 10, y = pos.y * 10}
        |
        |  // A new instance of a bean can be created by postfixing a bean type or a bean value with a parameter list ().
        |  // Postfixing with a block {} creates a new derived atchetype instance.
        |  // Also works for variables and functions returning beans.
        |  segments = segments + tipStructure(hue = 1).radius * 4
        |}
        |
        |
        |// Function definition
        |// If the block does not have any non-temporary properties, a dynamic bean is not created, instead an instance of the derived bean is returned.
        |// TODO: Remove temp, introduce prop that is non-temporary, other values are automatically temporary.
        |testAvgFun := Num {
        |  temp a := 0
        |  temp b := 0
        |
        |  private temp sum := a + b
        |
        |  value = sum / 2
        |}
        |
        |averagedValues := testAvgFun(a=3, b=4)
        |
        |testPosFun := Pos {
        |  // Temp annotated variables are discarded after the constructor is run
        |  temp pos := Pos(0,0)
        |  temp deltaX := 0
        |  temp deltaY := 0
        |  temp scaleX := 1
        |  temp scaleY := 1
        |
        |  x = (pos.x + deltaX) * scaleX
        |  y = (pos.y + deltaY) * scaleY
        |}
        |
        |testPosFunResult1 := testPosFun(pos = Pos(5, 2), deltaX = 3, scaleY = 2)
        |
        |PosFun := Pos { temp t := 0 }
        |
        |posInterpolator := PosFun {
        |  temp aPos: Pos := Pos()
        |  temp bPos: Pos := Pos()
        |  x = mix(t, aPos.x, bPos.x)
        |  y = mix(t, aPos.y, bPos.y)
        |}
        |
        |// Test passing function or bean type as argument
        |funky := {
        |  a := 1
        |  b : TestBean := TestStructure
        |  c : testPosFun := testPosFun
        |  d : posFun := posInterpolator(aPos = Pos(x=1, y=2), bPos = Pos(x=20, y=10))
        |
        |  someProp := b(radius = 4)
        |  someOtherProp := c(scaleX = 4, deltaX = 0.5)
        |
        |  // When invoking a constructor on some archetype, the default values for definitions ( := ) are only calculated
        |  // if the archetype does not have any value for them, and if no parameter was specified for them.
        |  interpolatedPos := posFun(t = 0.4)
        |
        |}
        |
      """.stripMargin)

    // TODO: Figure out initialization order
    // -> put values in parenthesis when calling (x = 4) -> the parent is initialized with them.  the block {} is run after that.

    // TODO: Allow non-named parameters, assume order is declaration order?  Only use the values defined in the derived class, not parents?


    // TODO: To add to a collection (list etc), use +=


  }

}


case class Pos(var x : Double = 0, var y : Double = 0)

case class TestBean(var pos: Pos = null, var radius: Double = 1, var segments: Double = 2)