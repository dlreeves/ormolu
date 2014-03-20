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
  def toSql = left.toSql + " OR " + right.toSql
}

object Or {
  def ors(xs: BoolValueExpr*) = if (xs.size < 2) xs.head
    else xs map {x => BooleanPredicand(x.enclose):BoolValueExpr} reduceLeft (Or(_, _))
}

case class And(left: ValueExpr, right: ValueExpr) extends BoolValueExpr {
  def toSql = left.toSql + " AND " + right.toSql
}

object And {
  def ands(xs: BoolValueExpr*): BoolValueExpr = if (xs.size < 2) xs.head
    else xs map {x => BooleanPredicand(x.enclose):BoolValueExpr} reduceLeft (And(_, _))
}

case object True extends BoolValueExpr {
  def toSql = "TRUE"
}

case object False extends BoolValueExpr {
  def toSql = "FALSE"
}

case class BooleanPredicand(value: ParenValueExpr) extends BoolValueExpr {
  def toSql = value.toSql
}