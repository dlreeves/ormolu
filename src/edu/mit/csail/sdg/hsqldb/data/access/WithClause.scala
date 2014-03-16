package edu.mit.csail.sdg.hsqldb.data.access

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.QueryName

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/9/11
 * Time: 9:08 AM
 * To change this template use File | Settings | File Templates.
 */

trait WithClause extends HsqlDbExpr
case class With(withList: Seq[(QueryName, QueryExpr)]) extends WithClause{
  def toSql = "WITH " + (for ((queryName, queryExpr) <- withList)
  yield "%s AS (%s)" format (queryName.toSql, queryExpr.toSql)).mkString(", ")
}

case class WithRecursive(withList: Seq[(QueryName, QueryExpr)]) extends WithClause{
  def toSql = "WITH RECURSIVE" + (for ((queryName, queryExpr) <- withList)
  yield "%s AS (%s)" format (queryName.toSql + queryName.params, queryExpr.toSql)).mkString(", ")
}