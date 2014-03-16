package edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 4:45 PM
 * To change this template use File | Settings | File Templates.
 */

case class CaseSpec (searchWhens: Seq[(BoolValueExpr, ValueExpr)], elseClause: Option[ValueExpr] = None ) extends NonParenValueExpr{
  def toSql = {
    val toSqlSearchWhens = for{(condition, result) <- searchWhens}
    yield "WHEN %s THEN %s" format (condition.toSql, result.toSql)

    val toSqlElse = for (elseC <- elseClause) yield "ELSE " + elseC.toSql

    "CASE %s %s\n END" format (toSqlSearchWhens mkString ("", "\n", "\n"), toSqlElse getOrElse "")
  }
}