package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef

case object Iden extends Relation {
  def name: String = "iden"
  override val arity = 2

  override def projection = Univ.projection ++ Univ.projection
  override def filter = Univ.filter
  override def tables = Univ.tables
}