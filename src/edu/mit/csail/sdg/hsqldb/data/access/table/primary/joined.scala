package edu.mit.csail.sdg.hsqldb.data.access.table.primary

import edu.mit.csail.sdg.hsqldb.syntax.value.BoolValueExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 11:33 PM
 * To change this template use File | Settings | File Templates.
 */

object joined {
  case class ParenJoin(table: JoinedTable) extends TablePrimNoAs {
  def toSql = "(" + table.toSql + ")"
}
  trait  JoinedTable extends TablePrimNoAs{
    def left: TablePrim
    def right: TablePrim
    def enclose = ParenJoin(this)
  }

  case class CrossJoin(left: TablePrim, right: TablePrim) extends JoinedTable {
    def toSql = "%s CROSS JOIN %s" format (left.toSql, right.toSql)
  }

  case class JoinOn(left: TablePrim, right: TablePrim, boolExpr: BoolValueExpr) extends JoinedTable {
    def toSql = "%s JOIN %s ON (%s)" format (left.toSql, right.toSql, boolExpr.toSql)
  }
}