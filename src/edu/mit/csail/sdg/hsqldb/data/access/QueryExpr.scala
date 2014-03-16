package edu.mit.csail.sdg.hsqldb.data.access

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 7:48 PM
 * To change this template use File | Settings | File Templates.
 */

case class Query(queryExpr: QueryExpr, withClause: Option[WithClause] = None) extends QueryExpr {
  def toSql = {
    val withClauseAsSql = for( clause <- withClause) yield clause.toSql

    withClauseAsSql.mkString + queryExpr.toSql
  }
}
trait QueryExpr extends HsqlDbExpr