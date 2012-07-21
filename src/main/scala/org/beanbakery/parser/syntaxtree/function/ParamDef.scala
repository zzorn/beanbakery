package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.parser.syntaxtree.Expr

/**
 *
 */
case class ParamDef(kind: Kind, name: Symbol, default: Option[Expr]) {

}