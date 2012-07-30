package org.beanbakery.parser

import scala.Predef._
import syntaxtree._
import bean.{IfExpr, VarRef}
import bool._
import bool.BoolNot
import bool.BoolOp
import bool.BoolOp
import bool.ComparisonOp
import bool.EqualityComparisonOp
import function._
import function.CallParam
import function.FunCall
import function.ParamDef
import kind.FunctionKind
import kind.SimpleKind
import kind.{FunctionKind, SimpleKind, Kind}
import num.Num
import num.NumNeg
import num.NumOp
import num.{Num, NumNeg, NumOp}
import syntaxtree.Def
import syntaxtree.Def
import syntaxtree.Import
import syntaxtree.Import
import syntaxtree.Module
import syntaxtree.Module
import syntaxtree.Parens
import syntaxtree.PathRef
import org.beanbakery.utils.language.LanguageParser
import syntaxtree.PathRef
import org.beanbakery.parser.Block
import syntaxtree.StringConst

/**
 * Parses beans from input files
 */
class BeanParser(/*beanFactory: BeanFactory*/) extends LanguageParser[Module] {

  val FUN = registerKeyword("fun")
  val VAL = registerKeyword("val")
  val MODULE = registerKeyword("module")
  val IMPORT = registerKeyword("import")
  val RETURN = registerKeyword("return")
  val AND = registerKeyword("and")
  val OR = registerKeyword("or")
  val XOR = registerKeyword("xor")
  val NOT = registerKeyword("not")
  val FALSE = registerKeyword("false")
  val TRUE = registerKeyword("true")
  val LIST = registerKeyword("List")
  val IF = registerKeyword("if")
  val THEN = registerKeyword("then")
  val ELSE = registerKeyword("else")

  registerDelimiters(
    "=", ",", ":", ";", ".",
    "(", ")", "[", "]", "{", "}",
    "->", "=>",
    "+", "-", "*", "/", "^",
    "<", ">", "<=", ">=", "!=", "==", "<>"
  )

  // Module
  def rootParser: PackratParser[Module] = moduleDef

  private lazy val moduleDef: PackratParser[Module] =
    MODULE ~> identifier("module identifier") ~ ("{" ~> imports) ~ definitions <~
      ("}" | failure("illegal import or definition")) ^^
      {case na ~ im ~ de => Module(na, im, de)}


  // Imports
  private lazy val imports: PackratParser[List[Import]] = rep(imp)
  private lazy val imp: PackratParser[Import] =
    IMPORT ~> path ~ opt("." ~> "*") <~ opt(";") ^^ {
      case p ~ star => Import(p, star.isDefined)
    }

  // Path
  private lazy val path: PackratParser[PathRef] =
    rep1sep(pathElement, ".") ^^ {
      case p => PathRef(p)
    }

  private lazy val pathElement: PackratParser[Symbol] = identifier("path name")

  // Definitions
  private lazy val definitions: PackratParser[List[Def]] =
    rep(definition)

  private lazy val definition: PackratParser[Def] =
    VAL ~> identifier("definition name") ~ typeTag ~ ("=" ~> expression) <~ opt(";") ^^ {case i ~ t ~ e => Def(i, t, e)}



  // Types
  private lazy val typeTag: PackratParser[Option[Kind]] =
    opt(":" ~> typeDesc)

  private lazy val typeDesc: PackratParser[Kind] =
    simpleType |
    funType |
    failure("illegal start of type")


  private lazy val simpleType: PackratParser[Kind] =
    identifier("type name") ^^ {SimpleKind(_)}

  private lazy val funType: PackratParser[FunctionKind] =
    listSep("(", typeDesc, ")", "parameter types") ~ ("->" ~> typeDesc) ^^ {case p ~ r => FunctionKind(p, r)}

  // Expressions
  private lazy val expression: PackratParser[Expr] =
    block |
    boolExpr |
    failure("illegal start of expression")


  // Block expression
  private lazy val block: PackratParser[Block] =
    "{" ~> definitions ~ (RETURN ~> expression) <~ "}" ^^ {case d ~ r => Block(d, r)}


  // Boolean expressions
  private lazy val boolExpr: PackratParser[Expr] =
    xorExpr ~ rep("or" ~ xorExpr) ^^ {case first ~ rest => processOperations(first, rest)}

  private lazy val xorExpr: PackratParser[Expr] =
    andExpr ~ rep("xor" ~ andExpr) ^^ {case first ~ rest => processOperations(first, rest)}

  private lazy val andExpr: PackratParser[Expr] =
    notExpr ~ rep("and" ~ notExpr) ^^ {case first ~ rest => processOperations(first, rest)}

  private lazy val notExpr: PackratParser[Expr] =
    NOT ~> equalExpr ^^ {BoolNot(_)} |
    equalExpr

  // Equality expressions
  private lazy val equalExpr: PackratParser[Expr] =
    compareExpr ~ "==" ~ compareExpr ^^ {case a ~ op ~ b => EqualityComparisonOp(a, '== , b)} |
    compareExpr ~ "!=" ~ compareExpr ^^ {case a ~ op ~ b => EqualityComparisonOp(a, '!= , b)} |
    compareExpr


  // Comparison expressions
  private lazy val compareExpr: PackratParser[Expr] =
    mathExpr ~ comparisonOp ~ mathExpr ~ comparisonOp ~ mathExpr ^^ {case a~op1~b~op2~c => ComparisonOp(a, op1, b, op2, c)} |
    mathExpr ~ comparisonOp ~ mathExpr ^^ {case a~op~b => ComparisonOp(a, op, b)} |
    mathExpr

  private lazy val comparisonOp: PackratParser[Symbol] = (">" | ">=" | "<" | "<=") ^^ {Symbol(_)}

  // Math expressions
  private lazy val mathExpr: PackratParser[Expr] =
    term ~ rep(("+" | "-") ~ term) ^^ {case first ~ rest => processOperations(first, rest)}

  private lazy val term: PackratParser[Expr] =
    factor ~ rep(("*" | "/") ~ factor) ^^ {case first ~ rest => processOperations(first, rest)}

  private lazy val factor: PackratParser[Expr] =
    unaryNegation ~ rep("^" ~ unaryNegation) ^^ {case first ~ rest => processOperations(first, rest)}

  private lazy val unaryNegation: PackratParser[Expr] =
    "-" ~> atom ^^ {NumNeg(_)} |
    atom

  private lazy val atom: PackratParser[Expr] =
    ifExpression |
    call |
    ref |
    literal |
    parens


  // If expression
  private lazy val ifExpression: PackratParser[IfExpr] =
    IF ~> boolExpr ~ (THEN ~> expression) ~ (ELSE ~> expression) ^^ {case c~t~e => IfExpr(c, t, e)}

  // Function calls
  private lazy val call: PackratParser[FunCall] =
    expression ~ ( "(" ~> repsep(argument, ",") <~ ")" ) ^^ {case e~p => FunCall(e, p)}

  private lazy val argument: PackratParser[CallParam] =
    opt( identifier("argument name") <~ "=") ~ expression ^^ {case n~v => CallParam(n, v)}


  // Value / variable references
  private lazy val ref: PackratParser[VarRef] =
    path ^^ {VarRef(_)}


  // Literals
  private lazy val literal: PackratParser[Expr] =
    numLiteral |
    funLiteral |
    stringLiteral |
    booleanLiteral |
    failure("Literal expected")


  // Simple literals
  private lazy val stringLiteral: PackratParser[Expr] =
    quotedString ^^ {StringConst(_)}

  private lazy val booleanLiteral: PackratParser[Expr] =
    TRUE ^^ (x => BoolTrue) |
    FALSE ^^ (x => BoolFalse)

  private lazy val numLiteral: PackratParser[Expr] =
    doubleNumber ^^ {Num(_)}


  // Function literal
  private lazy val funLiteral: PackratParser[Fun] =
    FUN ~> listSep("(", parameter, ")", "parameters") ~ typeTag ~ ("=>" ~> expression) ^^ {case p~t~e=> ExpressionFun(p, t, e)}

  private lazy val parameter: PackratParser[ParamDef] =
    identifier("parameter name") ~ typeTag ~ opt("=" ~> expression) ^^ {case n~t~d=> new ParamDef(n, t, d)}


  // Parenthesis
  private lazy val parens: PackratParser[Expr] =
    "(" ~> expression <~ ")" ^^ {Parens(_)}


  // Helper methods
  private def processOperations(first: Expr, rest: List[~[String, Expr]]): Expr = {
    if (rest.isEmpty) first
    else {
      val next = rest.head
      val opChar = next._1
      val other = processOperations(next._2, rest.tail)
      opChar match {
        case "+" => NumOp(first, '+ , other)
        case "-" => NumOp(first, '- , other)
        case "*" => NumOp(first, '* , other)
        case "/" => NumOp(first, '/ , other)
        case "^" => NumOp(first, '^ , other)
        case AND => BoolOp(first, 'and , other)
        case OR  => BoolOp(first, 'or , other)
        case XOR => BoolOp(first, 'xor , other)
      }
    }
  }

  // Identifier
//  private lazy val identifier: PackratParser[Symbol] = ident ^^ {Symbol(_)}

  private def identifier(name: String): PackratParser[Symbol] =
    ident ^^ {Symbol(_)} |
    failure("Expected " + name)

  private def listSep[T](start: String, content: PackratParser[T], end: String, name: String): PackratParser[List[T]] =
    start ~> repsep(content, ",") <~ end  |
    failure("Expected list of " + name)


}
