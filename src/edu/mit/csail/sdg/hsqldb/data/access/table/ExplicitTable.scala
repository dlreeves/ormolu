package edu.mit.csail.sdg.hsqldb.data.access.table

import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:21 PM
 * To change this template use File | Settings | File Templates.
 */

case class ExplicitTable(tableName: TableRef) extends QueryExpr {
  def toSql = "TABLE " + tableName.toSql
}