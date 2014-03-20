package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.data.access.table.Values
import edu.mit.csail.sdg.hsqldb.syntax.literal.NumericLiteral
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.Star
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.syntax.value.False


/**
 * Empty is the AtomSet with no elements. It represents the bottom of the base type system. 
 */
case object Empty extends AtomSet("nothing") {
  val createTable = "CREATE TABLE %s (col1 INTEGER PRIMARY KEY)" format tableRef.toSql
  override def query = querySpec

  override def filter = False :: Nil
  override def tables = Vector(Subquery(Values(Vector(Vector(NumericLiteral(-1))))).as(relationName, columns))

}