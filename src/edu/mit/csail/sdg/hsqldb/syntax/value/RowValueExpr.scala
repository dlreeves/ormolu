package edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 11:33 AM
 * To change this template use File | Settings | File Templates.
 */

trait RowValueExpr extends NonParenValueExpr

case class SimpleRowValueExpr(value: ValueExpr) extends RowValueExpr{
  val toSql = value.toSql
}

case class ExplicitRowValueExpr(values: ValueExpr*) extends RowValueExpr{
  val toSql = (values map {_.toSql}) mkString ("(", ", ", ")")
}