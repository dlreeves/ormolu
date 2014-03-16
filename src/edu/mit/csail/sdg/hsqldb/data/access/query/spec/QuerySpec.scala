package edu.mit.csail.sdg.hsqldb.data.access.query.spec

import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 7:53 PM
 * To change this template use File | Settings | File Templates.
 */

case class QuerySpec(selectList: SelectList, tableExpr: TableExpr) extends QueryExpr {
  def toSql = "SELECT DISTINCT %s %s" format (selectList.toSql, tableExpr.toSql)
}





