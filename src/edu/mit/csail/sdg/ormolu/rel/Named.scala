
package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.data.access.table.ExplicitTable
import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr

/**
 * A named entity in Ormolu
 */
abstract class Named(override val arity: Int) extends Relation{
  def name: String

  def tableRef: TableRef

  def createTable: String

  def createAddProcedure: String = ""

  override def query: QueryExpr = ExplicitTable(tableRef)

  override def tables = Vector(tableRef as relationName)
}