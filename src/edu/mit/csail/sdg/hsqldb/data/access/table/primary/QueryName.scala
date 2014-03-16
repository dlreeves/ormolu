package edu.mit.csail.sdg.hsqldb.data.access.table.primary

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 11:07 PM
 * To change this template use File | Settings | File Templates.
 */

case class QueryName(name: String, columns: Seq[String] = Nil) extends TablePrimNoAs{
  def toSql = name
  
  def params = columns.mkString("(", ", ", ")")
}