package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * The disjunction (||) of two Formulas. left || right is true when either left or right is true.
 */
case class Or(left: Formula, right: Formula) extends Formula {
  override def toString: String = left + " or " + right

  override def boolExpr = value.Or(left.boolExpr, right.boolExpr)
}