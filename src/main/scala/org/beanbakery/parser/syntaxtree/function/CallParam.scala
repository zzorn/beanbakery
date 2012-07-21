package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.Expr

/**
 * A function call parameter.  May be named or not.
 */
case class CallParam(id: Option[Symbol], expr: Expr) {

}