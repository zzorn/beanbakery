package org.beanbakery.parser

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import java.lang.String
import syntaxtree._
import bean.{IfExpr, BeanExpr, VarRef}
import bool._
import bool.BoolOp
import bool.ComparisonOp
import bool.BoolNot
import bool.EqualityComparisonOp
import function.{FunCall, CallParam, ParamDef, Fun}
import kind._
import kind.SimpleKind
import num._
import num.Num
import num.NumNeg
import num.NumOp
import propertyaccess.PropAccess
import syntaxtree.Module
import scala.Some
import org.parboiled.parserunners.ProfilingParseRunner
import org.parboiled.parserunners.ProfilingParseRunner.Report

/**
 * Parses an expression.
 * @Deprecated Too slow :/
 */
@Deprecated
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



  def parseDocumentString(expression: String): Doc = {
    //val runner = new ProfilingParseRunner[Doc](Document)
    val runner = ReportingParseRunner(Document)
    //val runner = TracingParseRunner(Document)
    val parsingResult = runner.run(expression)
    val expr: Doc = parsingResult.resultValue match {
      case e: Doc => e
      case null =>
        throw new ParsingException("Invalid expression:\n" + ErrorUtils.printParseErrors(parsingResult))
    }

    /*
    val report: Report = runner.getReport
    println(report.print())
    */

    expr
  }

  def Document = rule {
    Spacing ~ DocumentContents ~ EOI
  }

  def InputLine = rule {
    Spacing ~ Expression ~ EOI
  }

  def BooleanInputLine = rule {
    Spacing ~ BooleanExpression ~ EOI
  }

  def NumberInputLine = rule {
    Spacing ~ NumberExpression ~ EOI
  }

  def DocumentContents: Rule1[Doc] = rule {
    zeroOrMore(Definition) ~~> {new Doc(_)}
  }

  def Definition: Rule1[Def] = rule {
    AllowedName ~ "= " ~ Expression ~~> {Def(_, _)}
  }

  def Expression: Rule1[Expr] =
    BooleanExpression


  // Booleans

  def BooleanExpression: Rule1[Expr] = IfExpression

  def IfExpression: Rule1[Expr] = rule {
    "if " ~ OrExpr ~ "then " ~ OrExpr ~ "else " ~ Expression ~~> {IfExpr(_, _, _)} |
    OrExpr
  }

  def OrExpr: Rule1[Expr] = rule {
    AndExpr ~ zeroOrMore(
      "or " ~ AndExpr ~~> ((a: Expr, b: Expr) => BoolOp(a, 'or, b).asInstanceOf[Expr])
    )
  }

  def AndExpr: Rule1[Expr] = rule {
    NotExpr ~ zeroOrMore(
      "and " ~ NotExpr ~~> ((a: Expr, b: Expr) => BoolOp(a, 'and, b).asInstanceOf[Expr])
    )
  }

  def NotExpr: Rule1[Expr] = rule {
    "not " ~ EqualityExpr ~~> {
      expr => BoolNot(expr)
    } |
    EqualityExpr
  }

  def EqualityExpr: Rule1[Expr] = rule {
    ComparisonExpr ~ "== " ~ ComparisonExpr ~~> {(a, b) => EqualityComparisonOp(a, '==, b)} |
    ComparisonExpr ~ "!= " ~ ComparisonExpr ~~> {(a, b) => EqualityComparisonOp(a, '!=, b)} |
    ComparisonExpr
  }


  def ComparisonExpr: Rule1[Expr] = rule {
    NumberExpression ~ ComparisonSymbol ~ NumberExpression ~ ComparisonSymbol ~ NumberExpression ~~> {ComparisonOp(_, _, _, _, _)} |
    NumberExpression ~ ComparisonSymbol ~ NumberExpression ~~> {ComparisonOp(_, _, _)} |
    BooleanConst |
    NumberExpression
  }

  def ComparisonSymbol: Rule1[Symbol] = rule {
    group("<=" | ">=" | "<" | ">") ~> {Symbol(_)} ~ Spacing
  }


  def NumberExpression: Rule1[Expr] = TermExpr

  def TermExpr: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
      "+ " ~ Term ~~> ((a: Expr, b: Expr) => NumOp(a, '+, b).asInstanceOf[Expr]) |
      "- " ~ Term ~~> ((a: Expr, b: Expr) => NumOp(a, '-, b).asInstanceOf[Expr])
    )
  }

  def Term: Rule1[Expr] = rule {
    Exponentiation ~ zeroOrMore(
      "* " ~ Exponentiation ~~> ((a: Expr, b: Expr) => NumOp(a, '*, b).asInstanceOf[Expr]) |
      "/ " ~ Exponentiation ~~> ((a: Expr, b: Expr) => NumOp(a, '/, b).asInstanceOf[Expr])
    )
  }

  def Exponentiation: Rule1[Expr] = rule {
    Factor ~ zeroOrMore(
      "^ " ~ Factor ~~> ((a: Expr, b: Expr) => NumOp(a, '^, b).asInstanceOf[Expr])
    )
  }


  def Factor: Rule1[Expr] = rule {
    NumberConst |
    NumberNeg |
    InvocationOrAccess
  }


  def NumberNeg: Rule1[Expr] = rule {
    "- " ~ InvocationOrAccess ~~> {
      exp => NumNeg(exp)
    }
  }

  def InvocationOrAccess: Rule1[Expr] = rule {
    Invokable ~ zeroOrMore(
      ". " ~ AllowedName ~~> {(base: Expr, name: Symbol) => PropAccess(base, name).asInstanceOf[Expr]} |
      CallParameterList  ~~> {(base: Expr, paramList: List[CallParam]) => FunCall(base, paramList).asInstanceOf[Expr]}
    )
  }

  def CallParameterList: Rule1[List[CallParam]] = rule {
    "( " ~ zeroOrMore(CallParameter, separator = ", ") ~ ") "
  }

  def CallParameter: Rule1[CallParam] = rule {
    AllowedName ~ "= " ~ Expression ~~> {(name: Symbol, value: Expr) => CallParam(Some(name), value)} |
    Expression ~~> {CallParam(None, _)}
  }

  def Invokable: Rule1[Expr] = rule {
    FunctionLiteral |
    StringLiteral |
    VariableReference |
    Parens
  }

  def VariableReference: Rule1[Expr] = rule {
    AllowedName ~~> {VarRef(_)}
  }

  def Parens: Rule1[Expr] = rule {
    "( " ~ Expression ~ ") "
  }

  def FunctionLiteral: Rule1[Fun] = rule {
    "fun " ~ "( " ~ zeroOrMore(ParameterDefinition, separator = ", ") ~ ") " ~ "=> " ~Type ~ ExpressionBlock ~~>
      {Fun(_, _, _)}
  }

  def ParameterDefinition: Rule1[ParamDef] = rule {
    Type ~ AllowedName ~ optional("= " ~ Expression) ~~> {ParamDef(_, _, _)}
  }

  def ExpressionBlock: Rule1[Expr] = rule {
    "{ " ~ Expression ~ "} "
  }

  def Type: Rule1[Kind] = rule {
    "( " ~ zeroOrMore(Type, separator=", ") ~") " ~ "=> " ~ Type ~~> {FunctionKind(_, _)} |
    SimpleType
  }

  def SimpleType: Rule1[SimpleKind] = rule {
    AllowedName ~~> { id: Symbol =>
      id match {
        case BoolKind.id => BoolKind
        case NumKind.id => NumKind
        case _ => SimpleKind(id)
      }
    }
  }



  def AllowedName: Rule1[Symbol] = rule {
    group(LetterOrUnderscore ~ zeroOrMore(LetterOrUnderscore | Digit)) ~> {Symbol(_)} ~ Spacing
  }

  def Keyword = rule {
    "if" | "then" | "else" |
    "def" | "fun" | "val" |
    "and" | "or" | "not" | "xor" |
    "false" | "true" |
    "for"
  }

  def LetterOrUnderscore = rule {
    "a" - "z" | "A" - "Z" | "_"
  }

  def BooleanConst: Rule1[BoolExpr] = rule {
    "true "  ~> {_ => BoolTrue} |
    "false " ~> {_ => BoolFalse}
  }

  def NumberConst: Rule1[NumExpr] = rule {
    group(Integer ~ optional(Fraction)) ~> (s => {Num(s.toDouble)}) ~ Spacing
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


  def StringLiteral: Rule1[StringConst] = rule {
    "\"" ~ zeroOrMore(Character) ~> StringConst ~ "\" "
  }

  def Character: Rule0 = rule { EscapedChar | NormalChar }
  def EscapedChar: Rule0 = rule { "\\" ~ (anyOf("\"\\/bfnrt") | Unicode) }
  def NormalChar: Rule0 = rule { !anyOf("\"\\") ~ ANY }
  def Unicode: Rule0 = rule { "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit }
  def HexDigit: Rule0 = rule { "0" - "9" | "a" - "f" | "A" - "Z" }

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
    if (string.endsWith(" "))
      str(string.substring(0, string.length - 1)) ~ Spacing
    else
      str(string)


}