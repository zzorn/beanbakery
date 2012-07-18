package org.beanbakery.parser

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import java.lang.String
import syntaxtree._
import bool._
import bool.BoolOp
import bool.BoolVarRef
import bool.ComparisonOp
import bool.EqualityComparisonOp
import bool.EqualityComparisonOp
import bool.BoolNot
import bool.BoolNot
import num._
import num.NumIf
import num.NumOp
import num.NumVarRef
import scala.Some
import scala.Some
import org.beanbakery.BakeryContext


/**
 * Parses an expression.
 */
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

  def InputLine = rule {
    Expression ~ WhiteSpace ~ EOI
  }

  def BooleanInputLine = rule {
    BooleanExpression ~ WhiteSpace ~ EOI
  }

  def NumberInputLine = rule {
    NumberExpression ~ WhiteSpace ~ EOI
  }


  def Expression: Rule1[Expr] = BooleanExpression | NumberExpression

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
    WhiteSpace ~ group("<=" | ">=" | "<" | ">") ~> {
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
    NumberIf |
    NumberFunCall |
    NumberConst |
    NumberParens |
    NumberVar |
    NumberNeg
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
    Identifier ~ " (" ~ zeroOrMore(NumberExpression, separator = " ,") ~ " )" ~~> {
        (ident: Symbol, params: List[NumExpr]) =>
          NumFunCall(ident, params).asInstanceOf[NumExpr]
      }
  }

  def AllowedName: Rule1[Symbol] = Identifier

  // TODO: Exclude keywords
  def Identifier: Rule1[Symbol] = rule {
    WhiteSpace ~ group(LetterOrUnderscore ~ zeroOrMore(LetterOrUnderscore | Digit)) ~> {
      s => Symbol(s)
    }
  }

  def LetterOrUnderscore = rule {
    "a" - "z" | "A" - "Z" | "_"
  }

  def NumberConst: Rule1[NumExpr] = rule {
    WhiteSpace ~ group(Integer ~ optional(Fraction)) ~> (s => {
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

  def WhiteSpace: Rule0 = rule {
    zeroOrMore(anyOf("\n\r\t\f ").label("whitespace"))
  }

  def NonNewlineWhiteSpace: Rule0 = rule {
    zeroOrMore(anyOf("\t\f ").label("NonNewlineWhitespace"))
  }


  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with a blank.
   */
  override implicit def toRule(string: String) =
    if (string.startsWith(" "))
      WhiteSpace ~ str(string.substring(1))
    else
      str(string)


}