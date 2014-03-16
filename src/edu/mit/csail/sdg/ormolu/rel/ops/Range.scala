package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.TableStar
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinOn
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.value.SimpleRowValueExpr
import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * The Range restriction (:>) of two relations. left :> right contains the tuples of left that end with an element in right. right must be a set (arity = 1)
 */
case class Range(left: Relation, right: Relation) extends Relation {
  require(right.arity == 1,
    "The right arguement of Domain restriction must have arity 1, it has arity " + left.arity)

  override def arity: Int = left.arity
  override def toString: String = left + " :> " + right

  override def query = {
    val selectList = SeqSelectSubList(TableStar(left.relationTableRef) :: Nil)

    val tableJoin = JoinOn(left.tablePrim, right.tablePrim, Comparison(
      SimpleRowValueExpr(left.tableColumns.last),
      Comparison.Equals,
      SimpleRowValueExpr(right.tableColumns.head)))

    QuerySpec(selectList, TableExpr(tableJoin :: Nil))
  }
}