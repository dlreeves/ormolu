package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * The negation (!) of a Formula. !formula is true when formula is false.
 */
case class Not(formula: Formula) extends Formula {
  override def toString: String = "!" + formula

  override def boolExpr = value.Not(formula.boolExpr)
}