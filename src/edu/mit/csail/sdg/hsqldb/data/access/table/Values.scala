package edu.mit.csail.sdg.hsqldb.data.access.table

import edu.mit.csail.sdg.hsqldb.syntax.value.ValueExpr
import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:12 PM
 * To change this template use File | Settings | File Templates.
 */

case class Values(valueList: Seq[Seq[ValueExpr]]) extends QueryExpr{
  def toSql = {
    val valueListSql = for{values <- valueList} yield values.map(_.toSql).mkString("(", ", ", ")")

    "VALUES " + valueListSql.mkString(", ")
  }
}