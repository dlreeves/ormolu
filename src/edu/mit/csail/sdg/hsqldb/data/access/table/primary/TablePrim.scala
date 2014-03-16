package edu.mit.csail.sdg.hsqldb.data.access.table.primary

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr

sealed trait TablePrim extends HsqlDbExpr

trait TablePrimNoAs extends TablePrim {
  def as(name: String, columnNames: Seq[String] = Nil) = TablePrimWithAs(this, name, columnNames)
}

case class TablePrimWithAs(table: TablePrimNoAs, correlationName: String, derivedColumns: Seq[String] = Nil) extends TablePrim {
  def toSql = "%s AS %s" + toSqlDerivedColumns format (table.toSql, correlationName)

  def toSqlDerivedColumns = {
    val columns = derivedColumns
    if (columns.isEmpty)
      ""
    else
      derivedColumns.mkString("(", ", ", ")")
  }
}