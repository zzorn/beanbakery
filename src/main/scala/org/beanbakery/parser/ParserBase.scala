package org.beanbakery.parser

import org.parboiled.scala.Parser
import java.io.{InputStream, File}
import syntaxtree.Expr
import org.parboiled.common.FileUtils
import org.beanbakery.BakeryContext

/**
 *
 */
@Deprecated
trait ParserBase extends Parser {

  def parseFile(inputFile: File): Expr = {
    parseString(FileUtils.readAllText(inputFile))
  }

  def parseStream(stream: InputStream): Expr = {
    parseString(FileUtils.readAllText(stream))
  }

  def parseFileUsingName(inputFile: String): Expr = {
    parseString(FileUtils.readAllText(inputFile))
  }

  def parseString(expression: String): Expr

}