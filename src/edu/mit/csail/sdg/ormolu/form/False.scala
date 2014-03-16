package edu.mit.csail.sdg.ormolu.form

import edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * A Formula in Ormolu that is known to evaluate to false
 */
case object False extends Formula {
  override val boolExpr = value.False
}