package org.beanbakery.parser.syntaxtree.kind

import org.beanbakery.utils.ParameterChecker

/**
 *
 */
case class SimpleKind(id: Symbol) extends Kind {
  ParameterChecker.requireIsIdentifier(id, 'id)

}