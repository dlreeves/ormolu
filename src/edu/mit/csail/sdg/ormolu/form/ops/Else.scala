package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.form.Formula

/**
 * The alternative (else) of an implication. impl else formula is equivalent to impl && (!impl.cond => formula) 
 */
case class Else(impl: Implies, formula: Formula) extends Formula {
  override def toString: String = "%s else %s".format(impl, formula)

  override def boolExpr = (impl and (!impl.cond ==> formula)).boolExpr
}