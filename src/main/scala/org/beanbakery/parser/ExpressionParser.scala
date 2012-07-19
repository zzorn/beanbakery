package org.beanbakery.parser

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import java.lang.String
import syntaxtree._
import bool._
import bool.BoolNot
import bool.BoolOp
import bool.BoolOp
import bool.BoolVarRef
import bool.BoolVarRef
import bool.ComparisonOp
import bool.ComparisonOp
import bool.EqualityComparisonOp
import bool.EqualityComparisonOp
import bool.BoolNot
import bool.BoolNot
import bool.EqualityComparisonOp
import num._
import num.Num
import num.NumFunCall
import num.NumIf
import num.NumIf
import num.NumNeg
import num.NumOp
import num.NumOp
import num.NumVarRef
import num.NumVarRef
import scala.Some
import scala.Some
import org.beanbakery.BakeryContext
import scala.Some
import syntaxtree.BeanExpr
import syntaxtree.BlockDef
import syntaxtree.ExprType
import syntaxtree.NamedParam
import syntaxtree.PropAssignment
import syntaxtree.PropDef


/**
 * Parses an expression.
 */
// TODO: There may be issues with boolean variables and number variables.
class ExpressionParser() extends ParserBase {

  def parseString(expression: String): Expr = {
    val parsingResult = ReportingParseRunner(InputLine).run(expression)
    val expr = parsingResult.result match {
      case Some(e) => e
      case None => throw new ParsingException("Invalid expression:\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }

    expr
  }

  def parseDocumentString(expression: String): BlockDef = {
    val parsingResult = ReportingParseRunner(Document).run(expression)
    val expr = parsingResult.result match {
      case Some(e) => e
      case None => throw new ParsingException("Invalid expression:\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }

    expr
  }

  def Document = rule {
    BlockContents ~ Spacing ~ EOI
  }

  def InputLine = rule {
    Expression ~ Spacing ~ EOI
  }

  def BooleanInputLine = rule {
    BooleanExpression ~ Spacing ~ EOI
  }

  def NumberInputLine = rule {
    NumberExpression ~ Spacing ~ EOI
  }

  def Block: Rule1[BlockDef] = rule {
    " {" ~ BlockContents ~ " }"
  }

  def BlockContents: Rule1[BlockDef] = rule {
    zeroOrMore(Statement) ~~>
      {new BlockDef(_)}
  }

  def Statement: Rule1[Statement] = rule {
    Definition |
    Assignment
  }

  def Definition: Rule1[PropDef] = rule {
    AllowedName ~ optional(" :" ~ ExpressionType) ~ " :=" ~ Expression ~~>
      {PropDef(_, _, _)}
  }

  def Assignment: Rule1[PropAssignment] = rule {
    AllowedName ~ " =" ~ Expression ~~>
      {PropAssignment(_, _)}
  }

  def ExpressionType: Rule1[ExprType] = rule {
    AllowedName ~~> { id: Symbol =>
      id match {
        case BoolType.id => BoolType
        case NumType.id => NumType
        case BeanType.id => BeanType
        case _ => ExprType(id)
      }
    }
  }

  def Expression: Rule1[Expr] =
    BeanExpression |
    NumberExpression |
    BooleanExpression

  def BeanExpression: Rule1[BeanExpr] = rule {
    BeanExpressionWithBase |
    BeanExpressionWithoutBase
  }

  def BeanExpressionWithoutBase: Rule1[BeanExpr] = rule {
    Block ~~>
      {block => BeanExpr(None, None, Some(block))}
  }

  def BeanExpressionWithBase: Rule1[BeanExpr] = rule {
    ExpressionType ~ optional(NamedParameterList) ~ optional(Block)  ~~>
      {(exprType: ExprType, params: Option[List[NamedParam]], block: Option[BlockDef]) =>
        BeanExpr(Some(exprType), params, block)}
  }

  def NamedParameterList: Rule1[List[NamedParam]] = rule {
    " (" ~ zeroOrMore(NamedParameter, separator = " ,") ~ " )"
  }

  def NamedParameter: Rule1[NamedParam] = rule {
    AllowedName ~ " =" ~ Expression ~~> {NamedParam(_, _)}
  }

  def BooleanExpression: Rule1[BoolExpr] = OrExpr

  def OrExpr: Rule1[BoolExpr] = rule {
    AndExpr ~ zeroOrMore(
      " or" ~ AndExpr ~~> ((a: BoolExpr, b: BoolExpr) => BoolOp(a, 'or, b).asInstanceOf[BoolExpr])
    )
  }

  def AndExpr: Rule1[BoolExpr] = rule {
    NotExpr ~ zeroOrMore(
      " and" ~ NotExpr ~~> ((a: BoolExpr, b: BoolExpr) => BoolOp(a, 'and, b).asInstanceOf[BoolExpr])
    )
  }

  def NotExpr: Rule1[BoolExpr] = rule {
    " not" ~ EqualityExpr ~~> {
      expr => BoolNot(expr)
    } |
      EqualityExpr
  }

  def EqualityExpr: Rule1[BoolExpr] = rule {
    ComparisonExpr ~ " ==" ~ ComparisonExpr ~~> {
      (a, b) => EqualityComparisonOp(a, '==, b)
    } |
      ComparisonExpr ~ " !=" ~ ComparisonExpr ~~> {
        (a, b) => EqualityComparisonOp(a, '!=, b)
      } |
      ComparisonExpr
  }


  def ComparisonExpr: Rule1[BoolExpr] = rule {
    NumberExpression ~ ComparisonSymbol ~ NumberExpression ~ ComparisonSymbol ~ NumberExpression ~~> {
      (a, sym1, b, sym2, c) => ComparisonOp(a, sym1, b, sym2, c)
    } |
    NumberExpression ~ ComparisonSymbol ~ NumberExpression ~~> {
      (a, sym, b) => ComparisonOp(a, sym, b)
    } |
    Booleans
  }

  def ComparisonSymbol: Rule1[Symbol] = rule {
    Spacing ~ group("<=" | ">=" | "<" | ">") ~> {
      s => Symbol(s)
    }
  }

  def Booleans: Rule1[BoolExpr] = rule {
    BooleanParens |
    BooleanConst |
    BooleanVar
  }

  def BooleanParens: Rule1[BoolExpr] = rule {
    " (" ~ BooleanExpression ~ " )"
  }

  def BooleanVar: Rule1[BoolExpr] = rule {
    AllowedName ~~> {
      s => BoolVarRef(s)
    }
  }

  def BooleanConst: Rule1[BoolExpr] = rule {
    " true" ~> {
      _ => BoolTrue
    } |
    " false" ~> {
      _ => BoolFalse
    }
  }



  def NumberExpression: Rule1[NumExpr] = TermExpr

  def TermExpr: Rule1[NumExpr] = rule {
    Term ~ zeroOrMore(
      " +" ~ Term ~~> ((a: NumExpr, b: NumExpr) => NumOp(a, '+, b).asInstanceOf[NumExpr])
        | " -" ~ Term ~~> ((a: NumExpr, b: NumExpr) => NumOp(a, '-, b).asInstanceOf[NumExpr])
    )
  }

  def Term: Rule1[NumExpr] = rule {
    Exponentiation ~ zeroOrMore(
      " *" ~ Exponentiation ~~> ((a: NumExpr, b: NumExpr) => NumOp(a, '*, b).asInstanceOf[NumExpr])
        | " /" ~ Exponentiation ~~> ((a: NumExpr, b: NumExpr) => NumOp(a, '/, b).asInstanceOf[NumExpr])
    )
  }

  def Exponentiation: Rule1[NumExpr] = rule {
    Factor ~ zeroOrMore(
      " ^" ~ Factor ~~> ((a: NumExpr, b: NumExpr) => NumOp(a, '^, b).asInstanceOf[NumExpr])
    )
  }


  def Factor: Rule1[NumExpr] = rule {
    NumberConst |
    NumberNeg |
    NumberIf |
    NumberFunCall |
    NumberParens |
    NumberVar
  }


  def NumberNeg: Rule1[NumExpr] = rule {
    " -" ~ Factor ~~> {
      exp => NumNeg(exp)
    }
  }

  def NumberIf: Rule1[NumExpr] = rule {
    " if" ~ BooleanExpression ~ " then" ~ NumberExpression ~ " else" ~ NumberExpression ~~> {
      (c: BoolExpr, t: NumExpr, e: NumExpr) => NumIf(c, t, e).asInstanceOf[NumExpr]
    }
  }

  def NumberParens: Rule1[NumExpr] = rule {
    " (" ~ NumberExpression ~ " )"
  }

  def NumberVar: Rule1[NumExpr] = rule {
    AllowedName ~~> {
      s => NumVarRef(s)
    }
  }

  def NumberFunCall: Rule1[NumExpr] = rule {
    AllowedName ~ " (" ~ zeroOrMore(NumberExpression, separator = " ,") ~ " )" ~~> {
        (ident: Symbol, params: List[NumExpr]) =>
          NumFunCall(ident, params).asInstanceOf[NumExpr]
      }
  }

  def AllowedName: Rule1[Symbol] = Identifier

  // TODO: Exclude keywords
  def Identifier: Rule1[Symbol] = rule {
    Spacing ~ group(LetterOrUnderscore ~ zeroOrMore(LetterOrUnderscore | Digit)) ~> {
      s => Symbol(s)
    }
  }

  def LetterOrUnderscore = rule {
    "a" - "z" | "A" - "Z" | "_"
  }

  def NumberConst: Rule1[NumExpr] = rule {
    Spacing ~ group(Integer ~ optional(Fraction)) ~> (s => {
      Num(s.toDouble)
    })
  }

  def Integer: Rule0 = rule {
    optional("-") ~ (("1" - "9") ~ Digits | Digit)
  }

  // No leading zero in an integer, except if there is just one digit
  def Fraction: Rule0 = rule {
    "." ~ Digits
  }

  def Digits: Rule0 = rule {
    oneOrMore(Digit)
  }

  def Digit: Rule0 = rule {
    "0" - "9"
  }


  def Spacing: Rule0 = rule {
    // TODO: Figure out how to use SuppressNode

    zeroOrMore(EndOfLineComment | WhiteSpace)
  }

  def WhiteSpace: Rule0 = rule {
    oneOrMore(anyOf("\n\r\t\f ").label("whitespace"))
  }

  def EndOfLineComment: Rule0 = rule {
    "//" ~ zeroOrMore(noneOf("\r\n")) ~ ("\r\n" | "\r" | "\n" | EOI)
  }


  def NonNewlineWhiteSpace: Rule0 = rule {
    zeroOrMore(anyOf("\t\f ").label("NonNewlineWhitespace"))
  }


  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with a blank.
   */
  override implicit def toRule(string: String) =
    if (string.startsWith(" "))
      Spacing ~ str(string.substring(1))
    else
      str(string)


}