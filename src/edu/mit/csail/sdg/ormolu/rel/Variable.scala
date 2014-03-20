package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.syntax.reference.{ColumnRef, TableRef}
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.data.access.table.Values
import edu.mit.csail.sdg.hsqldb.syntax.literal.NumericLiteral

case class Variable(name: String, relation: Relation) extends Named(relation.arity){
  override def toString: String = name

  def tableRef = TableRef(name)

  def valueExprs = parameters.map(ColumnRef(_))
  val createTable = "DECLARE TABLE %s %s;" format (name, columns.mkString("(", " INTEGER, ", " INTEGER)"))
  def parameters = columns.map{name + "_" + _}
  val insertValues = "INSERT INTO %s VALUES (%s);" format (name, parameters.mkString(", "))

  override def query = querySpec

  override def projection = for (column <- columns) yield tableRef :@ column
  override def tables = Vector(Subquery(Values(Vector(Vector(NumericLiteral(-1))))).as(relationName, columns))
}