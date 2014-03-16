package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinOn
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinedTable
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.value.SimpleRowValueExpr
import edu.mit.csail.sdg.ormolu.rel.Relation

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

  override def query: QuerySpec = {
    val leftColumns = left.tableColumns
    val rightColumns = right.tableColumns

    val selectList = SeqSelectSubList(
      for ((column, asName) <- (leftColumns.init ++ rightColumns.tail) zip columns)
        yield DerivedColumn(column, Option(asName)))

    val tableJoin = JoinOn(left.tablePrim, right.tablePrim, Comparison(
      SimpleRowValueExpr(leftColumns.last),
      Comparison.Equals,
      SimpleRowValueExpr(rightColumns.head)))

    QuerySpec(selectList, TableExpr(tableJoin :: Nil))
  }
  override def tableColumns = for (DerivedColumn(valueExpr, _) <- query.selectList.subLists) yield valueExpr

  override def tablePrim = if (query.tableExpr.from.length == 1)
    query.tableExpr.from.head match {
      case table: JoinedTable => table.enclose
      case otherwise          => otherwise
    }
  else super.tablePrim
}