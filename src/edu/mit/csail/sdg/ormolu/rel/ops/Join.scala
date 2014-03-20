package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinOn
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinedTable
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.ormolu.rel.{Variable, Relation}
import edu.mit.csail.sdg.hsqldb.syntax.value.ExplicitRowValueExpr._
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison.Equals
import edu.mit.csail.sdg.hsqldb.syntax.predicate.AllComparison._
import edu.mit.csail.sdg.hsqldb.syntax.predicate.{AllComparison, Comparison}
import edu.mit.csail.sdg.hsqldb.syntax.value.{And, ExplicitRowValueExpr, SimpleRowValueExpr}

/**
 * The Join (.) of two relations. left.right of relations left and right is the relation you get by
 * taking every combination of a tuple from p and a tuple from q, and including their join, if it exists.
 * To join two tuples s1->...->sm t1->...->tn; you first check whether the last atom of the first tuple
 * (that is, sm) matches the first atom of the second tuple (that is, t1). If not, the result is empty, there is no join.
 * If so, it's the tuple that starts with the atoms of the first tuple, and finishes with the atoms of the second,
 * omitting just the matching atom: s1->...->sm-1->t2->...->tn
 */
case class Join(left: Relation, right: Relation) extends Relation {
  require(arity > 0,
    "Both the left and right relations are unary. Joining them would result in a relation with zero arity")
  override def arity: Int = left.arity + right.arity - 2
  override def toString: String = left + "." + right

  override def query = querySpec

  override def projection = left.projection.init ++ right.projection.tail
  override def filter = left.filter ++ right.filter :+ Comparison(
      SimpleRowValueExpr(left.projection.last),
      Comparison.Equals,
      SimpleRowValueExpr(right.projection.head))
  override def tables = left.tables ++ right.tables
}