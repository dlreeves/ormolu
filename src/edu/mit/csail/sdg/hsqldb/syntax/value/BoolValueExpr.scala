package edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 12:10 PM
 * To change this template use File | Settings | File Templates.
 */

trait BoolValueExpr extends NonParenValueExpr

case class Not(boolExpr: BoolValueExpr) extends BoolValueExpr {
  def toSql = "NOT " + boolExpr.enclose.toSql
}

case class Or(left: BoolValueExpr, right: BoolValueExpr) extends BoolValueExpr {
  def toSql = left.enclose.toSql + " OR " + right.enclose.toSql
}

case class And(left: BoolValueExpr, right: BoolValueExpr) extends BoolValueExpr {
  def toSql = left.enclose.toSql + " AND " + right.enclose.toSql
}

case object True extends BoolValueExpr {
  def toSql = "TRUE"
}

case object False extends BoolValueExpr {
  def toSql = "FALSE"
}

case class BooleanPredicand(value: NonParenValueExpr) extends BoolValueExpr {
  def toSql = value.toSql
}