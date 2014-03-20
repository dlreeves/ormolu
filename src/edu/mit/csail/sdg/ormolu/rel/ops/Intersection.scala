package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.setOps.{Intersect => SqlIntersect}
import edu.mit.csail.sdg.ormolu.rel.{Relation}
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr

/**
 * The intersection (&) of two relations. A tuple is in left & right when it is in left and in right
 */
case class Intersection(left: Relation, right: Relation) extends Relation {
  require(left.arity == right.arity,
    "The left relation has arity %s, while the right relation has arity %s. Both relations are expected to be the same".format(left.arity, right.arity))

  override def arity: Int = left.arity
  override def toString: String = left + " & " + right

  override def query = SqlIntersect(left.query, right.query)

  override def tables = Vector(Subquery(query).as(relationName, columns))
}