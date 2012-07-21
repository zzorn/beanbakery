package org.beanbakery.parser.syntaxtree.kind

/**
 *
 */
case class FunctionKind(parameterKinds: List[Kind], returnKind: Kind) extends Kind {

}