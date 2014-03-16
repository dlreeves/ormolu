package edu.mit.csail.sdg.hsqldb.syntax.predicate

import edu.mit.csail.sdg.hsqldb.data.access.Subquery

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 7:21 PM
 * To change this template use File | Settings | File Templates.
 */

case class Exists(query: Subquery) extends Predicate{
  def toSql = "EXISTS " + query.toSql
}