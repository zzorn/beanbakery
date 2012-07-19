package org.beanbakery.parser.syntaxtree

import org.beanbakery.utils.ParameterChecker

/**
 *
 */
case class ExprType(id: Symbol) {
  ParameterChecker.requireIsIdentifier(id, 'id)

}