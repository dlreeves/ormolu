package edu.mit.csail.sdg.hsqldb.data.access.query.spec

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.syntax.value.ValueExpr
import edu.mit.csail.sdg.hsqldb.syntax.reference
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:56 PM
 * To change this template use File | Settings | File Templates.
 */

trait SelectSubList extends HsqlDbExpr

case class DerivedColumn(valueExpr: ValueExpr, asName: Option[reference.ColumnName] = None) extends SelectSubList{
  def toSql = {
    val toSqlAsName = for (name <- asName) yield " AS " + name

    valueExpr.toSql + toSqlAsName.getOrElse("")
  }
}

case class TableStar(table: TableRef) extends SelectSubList{
  def toSql = table.toSql + ".*"
}