package edu.mit.csail.sdg.hsqldb.data.access.table.expression

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrim
import edu.mit.csail.sdg.hsqldb.syntax.value.BoolValueExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */

case class TableExpr(from: Seq[TablePrim], where: Option[BoolValueExpr] = None) extends HsqlDbExpr{
  def toSql = "FROM " +
    (from.map(_.toSql) mkString ", ") +
    (for(bool <- where) yield " WHERE " + bool.toSql).mkString
}