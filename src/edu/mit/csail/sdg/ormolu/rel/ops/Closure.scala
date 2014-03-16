package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.setOps.{Union => SqlUnion}
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.QuerySpec
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.TableStar
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinOn
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.QueryName
import edu.mit.csail.sdg.hsqldb.data.access.table.ExplicitTable
import edu.mit.csail.sdg.hsqldb.data.access.Query
import edu.mit.csail.sdg.hsqldb.data.access.WithRecursive
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.syntax.value.SimpleRowValueExpr
import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * The transitive Closure (^) of a binary relation. ^r is the smallest relation that contains r and is transitive.
 */
case class Closure(relation: Relation) extends Relation {
  require(relation.arity == 2,
    "The transitive Closure can only be used with a binary relation, r has arity " + relation.arity)

  override def arity: Int = relation.arity
  override def toString: String = "^" + relation

  override def query = {
    val queryName = QueryName(relationName, columns)
    val selectList = SeqSelectSubList(TableStar(relation.relationTableRef) :: Nil)

    val tableJoin = JoinOn(relation.tablePrim, queryName, Comparison(
      SimpleRowValueExpr(relation.tableColumns.last),
      Comparison.Equals,
      SimpleRowValueExpr(tableColumns.head)))
    val querySpec = QuerySpec(selectList, TableExpr(tableJoin :: Nil))

    val withClause = WithRecursive((queryName, SqlUnion(relation.query, querySpec)) :: Nil)
    Query(ExplicitTable(TableRef(queryName.name)), Option(withClause))
  }

}