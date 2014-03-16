package edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 11:32 AM
 * To change this template use File | Settings | File Templates.
 */

case object DynamicParameter extends NonParenValueExpr {
  val toSql = "?"
}