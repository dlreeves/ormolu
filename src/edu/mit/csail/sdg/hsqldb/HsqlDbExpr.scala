package edu.mit.csail.sdg.hsqldb

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:19 AM
 * To change this template use File | Settings | File Templates.
 */

trait HsqlDbExpr {
  def toSql: String
}