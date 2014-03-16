package edu.mit.csail.sdg.hsqldb.data.access


import edu.mit.csail.sdg.hsqldb.syntax.value.{NonParenValueExpr, RowValueExpr}
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrimNoAs

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */

  case class Subquery(queryExpr: QueryExpr) extends NonParenValueExpr with TablePrimNoAs with RowValueExpr{
    def toSql = "(" + queryExpr.toSql + ")"
  }