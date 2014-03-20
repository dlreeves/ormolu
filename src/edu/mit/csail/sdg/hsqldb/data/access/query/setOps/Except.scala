package edu.mit.csail.sdg.hsqldb.data.access.query.setOps

import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/9/11
 * Time: 9:02 AM
 * To change this template use File | Settings | File Templates.
 */

case class Except(left: QueryExpr, right: QueryExpr) extends QueryExpr{
  def toSql = "%s EXCEPT %s" format (left.toSql, right.toSql)
}

object Except {
  def excepts(xs: QueryExpr*) = if (xs.size < 2) xs.head
    else xs reduceLeft (Except(_, _))
}

case class ExceptAll(left: QueryExpr, right: QueryExpr) extends QueryExpr{
  def toSql = "%s EXCEPT ALL %s" format (left.toSql, right.toSql)
}