package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.value.SimpleRowValueExpr
import edu.mit.csail.sdg.ormolu.rel.{Relation}

/**
 * The Range restriction (:>) of two relations. left :> right contains the tuples of left that end with an element in right. right must be a set (arity = 1)
 */
case class Range(left: Relation, right: Relation) extends Relation {
  require(right.arity == 1,
    "The right arguement of Domain restriction must have arity 1, it has arity " + left.arity)

  override def arity: Int = left.arity
  override def toString: String = left + " :> " + right

  override def query = querySpec

  override def projection = left.projection
  override def filter = left.filter ++ right.filter :+ Comparison(
      SimpleRowValueExpr(left.projection.last),
      Comparison.Equals,
      SimpleRowValueExpr(right.projection.head))
  override def tables = left.tables ++ right.tables
}