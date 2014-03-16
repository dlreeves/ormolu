package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * The conjunction (&&) of two Formulas. left && right is true when both left and right are true.
 */
case class And(left: Formula, right: Formula) extends Formula {
  override def toString: String = left + " and " + right

  override def boolExpr = value.And(left.boolExpr, right.boolExpr)
}