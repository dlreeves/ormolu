package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.ormolu.rel.{Relation}
import edu.mit.csail.sdg.hsqldb.syntax.value.{SimpleRowValueExpr}

/**
 * The Domain restriction (<:) of two relations. left <: right contains those tuples of right that start with an element in left. left must be a set (arity = 1)
 */
case class Domain(left: Relation, right: Relation) extends Relation {
  require(left.arity == 1,
    "The left arguement of Domain restriction must have arity 1, it has arity " + left.arity)

  override def arity: Int = right.arity
  override def toString: String = left + " <: " + right

  override def query = querySpec

  override def projection = right.projection
  override def filter = left.filter ++ right.filter :+ Comparison(
      SimpleRowValueExpr(left.projection.head),
      Comparison.Equals,
      SimpleRowValueExpr(right.projection.head))
  override def tables = left.tables ++ right.tables

}