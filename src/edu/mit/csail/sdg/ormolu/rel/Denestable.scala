package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.syntax.reference.ColumnRef
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrim
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.{DerivedColumn, SeqSelectSubList, QuerySpec}
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.syntax.value.{And, BoolValueExpr}

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/31/11
 * Time: 5:24 PM
 * To change this template use File | Settings | File Templates.
 */

trait Denestable {
  def projection: Seq[ColumnRef]
  def filter: Seq[BoolValueExpr]
  def tables: Seq[TablePrim]
  final def querySpec: QuerySpec = {
    val where = if (filter.isEmpty) None else Some(And.ands(filter:_*))
    val derivedCols = projection map (DerivedColumn(_))
    QuerySpec(SeqSelectSubList(derivedCols), TableExpr(tables, where))
  }
}