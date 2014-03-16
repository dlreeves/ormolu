package edu.mit.csail.sdg.ormolu.form

import edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * A Formula in Ormolu that is known to evaluate to true
 */
case object True extends Formula {
 override val boolExpr = value.True
}