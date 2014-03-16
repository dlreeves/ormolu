package edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 11:43 AM
 * To change this template use File | Settings | File Templates.
 */

sealed trait AggregateFunction extends SetFunctionSpec
case object CountAll extends AggregateFunction {
  val toSql = "COUNT(*)"
}