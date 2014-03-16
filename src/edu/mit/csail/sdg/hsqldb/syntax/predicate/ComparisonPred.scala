package edu.mit.csail.sdg.hsqldb.syntax.predicate

import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison.Op
import edu.mit.csail.sdg.hsqldb.syntax.value.RowValueExpr
import edu.mit.csail.sdg.hsqldb.HsqlDbExpr

case class Comparison(left: RowValueExpr, op: Op, right: RowValueExpr) extends Predicate {
  def toSql = "%s %s %s" format (left.toSql, op.toSql, right.toSql)
}

object Comparison {
  sealed trait Op extends HsqlDbExpr

  case object Equals extends Op { val toSql = "=" }
  case object NotEquals extends Op { val toSql = "<>" }
  case object Less extends Op { val toSql = "<" }
  case object Great extends Op { val toSql = ">" }
  case object LessEq extends Op { val toSql = "<=" }
  case object GreatEq extends Op { val toSql = ">=" }
}