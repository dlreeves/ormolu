package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * The Transpose (~) of a binary relation. ~r takes the mirror image of r, forming a new relation by reversing the order of atoms in each tuple.
 */
case class Transpose(relation: Relation) extends Relation {
  require(relation.arity == 2,
    "Transpose can only be used with a binary relation, r has arity " + relation.arity)

  override def arity: Int = relation.arity
  override def toString: String = "~" + relation
  
  override def query = {
    val selectSubLists = for ((column, asName) <- relation.tableColumns.reverse zip columns) yield DerivedColumn(column, Option(asName))
    
    QuerySpec(SeqSelectSubList(selectSubLists), TableExpr(relation.tablePrim :: Nil))
  }
}