package edu.mit.csail.sdg.hsqldb.syntax

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.{TablePrimNoAs, TablePrim}
import edu.mit.csail.sdg.hsqldb.syntax.value.NonParenValueExpr
import edu.mit.csail.sdg.hsqldb.syntax.value.RowValueExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:31 AM
 * To change this template use File | Settings | File Templates.
 */

object reference {
  type SchemaName = String
  type TableName = String
  type ColumnName = String

  private [reference] sealed trait reference extends HsqlDbExpr
  implicit def stringToSchema(s: String) = SchemaRef(s)
  implicit def stringToTable(s: String) = TableRef(s)
  implicit def stringToColumn(s: String) = ColumnRef(s)

  implicit def tuple2ToTable(s: (String, String)) = SchemaRef(s._1) :@ s._2
  implicit def tuple2ToColumn(s: (String, String)) = TableRef(s._1) :@ s._2

  implicit def tuple3ToColumn(s: (String, String, String)) = SchemaRef(s._1) :@ s._2 :@ s._3



  case class SchemaRef(schema: SchemaName) extends reference{
    val toSql = schema

    def :@(table: TableName) = TableRef(table, Some(this))
  }

  case class TableRef(table: TableName, schema: Option[SchemaRef] = None) extends reference with TablePrimNoAs{
    val toSql = schema match {
      case Some(schemaRef) => schemaRef.toSql + "." + table
      case otherwise => table
    }

    def :@(column: ColumnName) = ColumnRef(column, Some(this))
  }

  case class ColumnRef(column: ColumnName, table: Option[TableRef] = None) extends reference with NonParenValueExpr with RowValueExpr{
    val toSql = table match {
      case Some(tableRef) => tableRef.toSql + "." + column
      case otherwise => column
    }
  }
}