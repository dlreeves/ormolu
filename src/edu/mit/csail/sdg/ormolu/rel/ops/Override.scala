package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.ormolu.rel.{Relation}
import edu.mit.csail.sdg.hsqldb.syntax.value.{SimpleRowValueExpr}


/**
 * The Override (++) of two relations. left ++ right is like the union, except that the tuples of right can replace the tuples of left rather than
 * just augmenting them. Any tuple in left that matches a tuple in right by starting with the same element is dropped. The relations left and right
 * can have any matching arity
 */
case class Override(left: Relation, right: Relation) extends Relation {
  require(left.arity == right.arity,
    "The left relation has arity %s, while the right relation has arity %s. Both relations are expected to be the same".format(left.arity, right.arity))

  override val arity: Int = left.arity
  override def toString: String = left + " ++ " + right

  private lazy val rewrite = OverrideHelper(left, right) + right
  override def query = rewrite.query

  override val tables = rewrite.tables

  private[Override] case class OverrideHelper(left:Relation, right:Relation) extends Relation {
  override val arity = left.arity
  override def query = querySpec
  override def projection = left.projection
  override def filter = left.filter ++ right.filter :+ Comparison(
      SimpleRowValueExpr(left.projection.head),
      Comparison.NotEquals,
      SimpleRowValueExpr(right.projection.head))
  override def tables = left.tables ++ right.tables
}
}

