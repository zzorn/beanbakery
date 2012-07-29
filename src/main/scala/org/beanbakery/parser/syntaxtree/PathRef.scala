package org.beanbakery.parser.syntaxtree

/**
 * Reference to a specific definition or module or package, relative to the project root or the current context.
 */
case class PathRef(path: List[Symbol]) {
  require(!path.isEmpty, "Path should not be empty")

  def isSingle = path.length == 1

  def head: Symbol = path.head

  override def toString = path.map(p => p.name).mkString(".")

  def lastName: Symbol = path.last

}