package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.ormolu.rel.Relation
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.CrossJoin

/**
 * The Product (->) of two relations. left -> right of two relations left and right is the relation you get by
 * taking every combination of a tuple from p and a tuple from q and concatenating them.
 */
case class Product(left: Relation, right: Relation) extends Relation {
  override def arity: Int = left.arity + right.arity
  override def toString: String = left + "->" + right

  override def query = {
    val leftColumns = left.tableColumns
    val rightColumns = right.tableColumns

    val selectList = SeqSelectSubList(
      for ((column, asName) <- (leftColumns ++ rightColumns) zip columns)
        yield DerivedColumn(column, Option(asName)))

    val tableJoin = CrossJoin(left.tablePrim, right.tablePrim)

    QuerySpec(selectList, TableExpr(tableJoin :: Nil))
  }
}