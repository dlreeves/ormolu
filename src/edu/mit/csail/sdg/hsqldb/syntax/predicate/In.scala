package edu.mit.csail.sdg.hsqldb.syntax.predicate
import edu.mit.csail.sdg.hsqldb.syntax.value.RowValueExpr
import edu.mit.csail.sdg.hsqldb.data.access.Subquery


/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 7:12 PM
 * To change this template use File | Settings | File Templates.
 */


case class In(left: RowValueExpr, right: Subquery) extends Predicate{
  def toSql = "%s IN %s" format (left.toSql, right.toSql)
}