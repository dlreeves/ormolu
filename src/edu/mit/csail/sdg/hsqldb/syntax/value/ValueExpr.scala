package edu.mit.csail.sdg.hsqldb.syntax.value

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:56 AM
 * To change this template use File | Settings | File Templates.
 */

sealed trait ValueExpr extends HsqlDbExpr

case class ParenValueExpr(nonParenValueExpr: NonParenValueExpr) extends ValueExpr {
  def toSql = "(" + nonParenValueExpr.toSql + ")"
}

trait NonParenValueExpr extends ValueExpr {
  def enclose = NonParenValueExpr.enclose(this)
}

trait SetFunctionSpec extends NonParenValueExpr

trait RoutineInvocation extends NonParenValueExpr

object NonParenValueExpr {
  val enclose = ParenValueExpr(_:NonParenValueExpr)
}



