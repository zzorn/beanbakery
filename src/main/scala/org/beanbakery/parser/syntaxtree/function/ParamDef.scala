package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class ParamDef(name: Symbol, kind: Option[Kind] = None, default: Option[Expr] = None) {

}