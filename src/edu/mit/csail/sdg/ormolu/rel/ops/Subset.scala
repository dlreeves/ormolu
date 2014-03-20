package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.ormolu.rel.{Variable, Relation}
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison.Equals
import edu.mit.csail.sdg.hsqldb.syntax.value.ExplicitRowValueExpr
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.syntax.predicate.{In, AllComparison, Comparison}

/**
 * The formula that checks if left is a subset (in) right. left in right is true when every tuple of left is also a tuple of right
 */
case class Subset(left: Relation, right: Relation) extends Formula {
  require(left.arity == right.arity,
    "The left relation has arity %s, while the right relation has arity %s. Both relations are expected to be the same left : %s, right: %s".format(left.arity, right.arity, left, right))

  override def toString: String = left + " in " + right

  override def boolExpr = No(left - right).boolExpr
}