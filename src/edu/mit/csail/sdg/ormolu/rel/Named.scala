
package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.table.ExplicitTable


/**
 * A named entity in Ormolu
 */
abstract class Named(override val arity: Int) extends Relation{
  override lazy val relationName: String = name
  override def relationTableRef = tableRef
  override def tablePrim = tableRef
  def name: String

  def tableRef: TableRef
  

  def asTableExpr: TableExpr = TableExpr(Vector(tableRef))

  def derivedColumns: Seq[DerivedColumn] = for (column <- columns) yield DerivedColumn( tableRef :@ column )

  def createTable: String

  def createAddProcedure: String = ""

  override def query = ExplicitTable(tableRef)
}