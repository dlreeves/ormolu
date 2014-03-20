package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.ormolu.rel.{Relation}


/**
 * The formula that checks if left is equal (=) to right. left = right is true when left and right have the same tuples
 */
case class Equality(left: Relation, right: Relation) extends Formula {
  require(left.arity == right.arity,
    "The left relation has arity %s, while the right relation has arity %s. Both relations are expected to be the same".format(left.arity, right.arity))

  override def toString: String = left + " = " + right

  override def boolExpr = {(left in right) and (right in left)}.boolExpr

}