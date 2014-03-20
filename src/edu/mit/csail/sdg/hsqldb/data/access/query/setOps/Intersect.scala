package edu.mit.csail.sdg.hsqldb.data.access.query.setOps

import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/9/11
 * Time: 9:03 AM
 * To change this template use File | Settings | File Templates.
 */

case class Intersect(left: QueryExpr, right: QueryExpr) extends QueryExpr{
  def toSql = "%s INTERSECT %s" format (left.toSql, right.toSql)
}

object Intersect {
  def intersects(xs: QueryExpr*) = if (xs.size < 2) xs.head
    else xs reduceLeft (Intersect(_, _))
}

case class IntersectAll(left: QueryExpr, right: QueryExpr) extends QueryExpr{
  def toSql = "%s INTERSECT ALL %s" format (left.toSql, right.toSql)
}