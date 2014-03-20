package edu.mit.csail.sdg.hsqldb.syntax.predicate

import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison.Op
import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.syntax.value.{ValueExpr, RowValueExpr}
import edu.mit.csail.sdg.hsqldb.data.access.Subquery

case class Comparison(left: ValueExpr, op: Op, right: ValueExpr) extends Predicate {
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

case class AllComparison(left: ValueExpr, op: Op, right: Subquery) extends Predicate {
  def toSql = "%s %s ALL %s" format (left.toSql, op.toSql, right.toSql)
}

case class AnyComparison(left: ValueExpr, op: Op, right: Subquery) extends Predicate {
  def toSql = "%s %s ANY %s" format (left.toSql, op.toSql, right.toSql)
}