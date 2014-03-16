package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef

case object Iden extends Named(2) {
  def name: String = "iden"

  def createTable = {
    val selectList = SeqSelectSubList(
      for (asName <- columns)
        yield DerivedColumn(Univ.tableColumns.head, Option(asName)))

    val query = QuerySpec(selectList, TableExpr(Univ.tablePrim :: Nil))

    "CREATE VIEW %s (col1, col2) AS %s" format (tableRef.toSql, query.toSql)
  }

  def tableRef = TableRef(name)
}