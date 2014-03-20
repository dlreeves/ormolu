package edu.mit.csail.sdg.hsqldb.data.access.query.setOps

import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/9/11
 * Time: 9:00 AM
 * To change this template use File | Settings | File Templates.
 */

case class Union(left: QueryExpr, right: QueryExpr) extends QueryExpr{
  def toSql = "%s UNION %s" format (left.toSql, right.toSql)
}

object Union {
  def unions(xs: QueryExpr*) = if (xs.size < 2) xs.head
    else xs reduceLeft (Union(_, _))
}

case class UnionAll(left: QueryExpr, right: QueryExpr) extends QueryExpr{
  def toSql = "%s UNION ALL %s" format (left.toSql, right.toSql)
}