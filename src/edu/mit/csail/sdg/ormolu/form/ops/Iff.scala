package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.form.Formula

/**
 * The bi-implication (<=>) of two Formulas. left <=> right is equivalent to (left => right) && (right => left)
 */
case class Iff(left: Formula, right: Formula) extends Formula {
  override def toString: String = left + " <=> " + right

  override def boolExpr = (left ==> right) and (right ==> left) boolExpr
}