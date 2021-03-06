package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.form.{ Formula, True, False }

/**
 * The implication (=>) of two Formula. left => right is true when left is false, or when left and right are true.
 * This is equivalent to !left || right
 */
case class Implies(left: Formula, right: Formula) extends Formula {
  val cond: Formula = left

  final def otherwise(f: Formula) = Else(this, f)

  override def toString: String = left + " => " + right

  override def boolExpr = (!left or right).boolExpr
}