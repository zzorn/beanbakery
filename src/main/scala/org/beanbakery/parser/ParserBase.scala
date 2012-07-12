package org.beanbakery.parser

import org.parboiled.scala.Parser
import java.io.{InputStream, File}
import syntaxtree.Expr
import org.parboiled.common.FileUtils

/**
 *
 */
trait ParserBase extends Parser {

  def parseFile(inputFile: File, rootContext: Context = new Context()): Expr = {
    parseString(FileUtils.readAllText(inputFile), rootContext)
  }

  def parseStream(stream: InputStream, rootContext: Context = new Context()): Expr = {
    parseString(FileUtils.readAllText(stream), rootContext)
  }

  def parseFileUsingName(inputFile: String, rootContext: Context = new Context()): Expr = {
    parseString(FileUtils.readAllText(inputFile), rootContext)
  }

  def parseString(expression: String, rootContext: Context = new Context()): Expr

}