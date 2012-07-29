package org.beanbakery

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import parser.BeanParser
import parser.syntaxtree.Const
import parser.syntaxtree.function.HardwiredFun2
import parser.syntaxtree.kind.{NumKind, SimpleKind}
import scala.math._

/**
 *
 */
class BeanBakeryTest  extends FunSuite{

  test("Creating a bean") {
    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Pos())
    bakery.addBeanClass('TestBean, classOf[TestBean])


    val posRecipe = new BeanRecipe(SimpleKind('Point))
    posRecipe.setExpression('x, Const(-100, NumKind))
    posRecipe.setExpression('y, Const(200, NumKind))

    val recipe = new BeanRecipe(SimpleKind('TestBean))
    recipe.setExpression('pos, posRecipe)
    recipe.setExpression('radius, Const(6.28, NumKind))
    recipe.setExpression('segments, Const(8, NumKind))

    val bean: TestBean = recipe.evaluate(new Scope()).asInstanceOf[TestBean]

    assert(bean.pos === new Pos(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)
  }

  test("Parse expression") {
    val parser = new BeanParser()

    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Pos())
    bakery.addBeanClass('TestBean, classOf[TestBean])

    val posRecipe = new BeanRecipe(SimpleKind('Point))
    posRecipe.setExpression('x, "-100", parser)
    posRecipe.setExpression('y, "pow(10, 2) + testFun(5*2, 2)", parser)

    val recipe = new BeanRecipe(SimpleKind('TestBean))
    recipe.setExpression('pos, posRecipe)
    recipe.setExpression('radius, "3.14 * a", parser)
    recipe.setExpression('segments, "2 * 2 + (if 1 > 0 then 2 else 0) ^ 2", parser)

    val context = new Scope(Map('a -> 2.0, 'testFun -> HardwiredFun2[Double, Double, Double]('a, 'b, NumKind, NumKind, NumKind, pow(_,_))))
    val bean: TestBean = recipe.evaluate(context).asInstanceOf[TestBean]

    assert(bean.pos === new Pos(-100, 200))
    assert(bean.radius === 6.28)
    assert(bean.segments === 8)

  }


  test("Parsing whole bean from text") {

    val bakery = new BeanBakery()
    bakery.addBeanCreator('Point, () => new Pos())
    bakery.addBeanClass('TestBean, classOf[TestBean])


    // Scope:
    //
    // During type checking:
    // - Pre-defined types
    // - Pre-defined functions
    //
    // Created and used during type checking
    // - Defined functions
    //
    // Used during function invocation / calculation
    // - global scope (pre-defined and visible parsed functions)
    // - parameters of enclosing functions
    // - defined local values in current and parent scopes
    //
    // For inner / anonymous functions
    // - Captured local scope at creation time
    // - Passed in parameters

    val doc1 = bakery.parseDocument(
      /*"""
        |// Default parameter expressions only have global definitions in scope.
        |TestBeanConstruct = fun (Num size = 10, Pos tip = Pos(x = 3)) => TestBean {
        |  // Scope in expression is global scope + parameter list of enclosing function definition
        |  TestBean (
        |    radius   = size * 3 ,
        |    segments = size * 2 ,
        |    pos      = Pos(tip.x, tip.y + size)
        |  )
        |}
        |
        |lerp = fun (Num t, Num a, Num b) => Num { a * (1 - t) + b * t }
        |
        |flowerFactory = fun (Color color = Color(1, 1, 1)) => (Num, Num) => Flower {
        |  fun (Num leaves = 4, Num petalSize = 4) => Flower {
        |    Flower(color, leaves)
        |  }
        |}
        |
        |makeTwig = fun (Num pos) => Twig { Twig(size = pos * 100) }
        |
        |funsky = fun ((Num, Num) => ((Num) => Num) => Num pos) => Twig { Twig(size = pos * 100) }
        |
        |
      """.stripMargin)
    */
    /*
      """
        |FirTree = fun (Num height = 10,
        |               (Num) => Shape foliageMaker = fun (Num pos)=>Foliage {Foliage(size=pos*10)} ) => Tree {
        |  //val Num branchLen = height * 0.2
        |
        |  Tree(
        |    flowers  = flowerFactory(Color(0.5, 0.2, 1)),
        |    twigs    = makeTwig,
        |    foliage  = foliageMaker,
        |    branches = fun (Num pos)=>Branch {
        |      Branch(
        |        texture = "FirTree",
        |        length  = branchLen,
        |        angle   = mix(pos, 0.2*Tau, 0.5*Tau + sin(height) )
        |      )
        |    }
        |  )
        |}
        |
        |
        |
      """.stripMargin)
*/

    /*

    space := " " | "\n"  etc
    identifier := identifierStart identifierMember*
    type := identifier | "(" (type, )* "->" type ")"
    number := integer ["." digits]
      integer := ["-"] digits
      digits := ("0".."9")+
    fun := "fun" "(" (paramDec, )* ")" type "=>" expr
      paramDec := type identifier ["=" expr]
    ref := identifier
    access := expr "." identifier
    call := expr "(" (param, )* ")"
      param := identifier "=" expr | expr

    +addition etc...

    expr := number | fun | access | call | ref


     */


      """
        |module foo {
        |
        |import utils.barfoo
        |import utils.foobar.*
        |
        |val FirTree = fun (height = 10,
        |               foliageMaker: Num = 2): Tree => {
        |  val branchLen = height * 0.2
        |
        |  return Tree(
        |    flowers  = flowerFactory(Color(0.5, 0.2, 1)),
        |    twigs    = makeTwig,
        |    foliage  = foliageMaker,
        |    branches = fun (pos: Num): Branch => Branch (
        |        texture = "FirTree",
        |        length  = branchLen,
        |        angle   = mix(pos, 0.2*Tau, 0.5*Tau + sin(height) )
        |      )
        |
        |  )
        |}
        |
        |}
        |
      """.stripMargin)


    // TODO: Figure out initialization order
    // -> put values in parenthesis when calling (x = 4) -> the parent is initialized with them.  the block {} is run after that.

    // TODO: Allow non-named parameters, assume order is declaration order?  Only use the values defined in the derived class, not parents?


    // TODO: To add to a collection (list etc), use +=


  }


  test("Test typing") {

    val bakery = new BeanBakery()
    val doc = bakery.parseDocument("""
      |module typetest {
      |
      |val avg = fun (a: Num, b: Num): Num => (a + b) / 2
      |
      |val avg13 = avg(1, 3)
      |
      |}
      |
    """.stripMargin)

    // TODO
//    assert( doc.getVal('avg13, new Scope(bakery)) === 2.0)
  }

}


case class Pos(var x : Double = 0, var y : Double = 0)

case class TestBean(var pos: Pos = null, var radius: Double = 1, var segments: Double = 2)