package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.setOps.{ Union => SqlUnion }
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.TableStar
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinOn
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.value.SimpleRowValueExpr
import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * The Override (++) of two relations. left ++ right is like the union, except that the tuples of right can replace the tuples of left rather than
 * just augmenting them. Any tuple in left that matches a tuple in right by starting with the same element is dropped. The relations left and right
 * can have any matching arity
 */
case class Override(left: Relation, right: Relation) extends Relation {
  require(left.arity == right.arity,
    "The left relation has arity %s, while the right relation has arity %s. Both relations are expected to be the same".format(left.arity, right.arity))

  override def arity: Int = left.arity
  override def toString: String = left + " ++ " + right

  override def query = {
    val selectList = SeqSelectSubList(TableStar(left.relationTableRef) :: Nil)

    val tableJoin = JoinOn(left.tablePrim, right.tablePrim, Comparison(
      SimpleRowValueExpr(left.tableColumns.head),
      Comparison.NotEquals,
      SimpleRowValueExpr(right.tableColumns.head)))

    SqlUnion(QuerySpec(selectList, TableExpr(tableJoin :: Nil)), right.query)
  }
}