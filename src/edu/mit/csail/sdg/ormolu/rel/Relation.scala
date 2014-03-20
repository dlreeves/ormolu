package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.ormolu.Expression
import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrim
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.ormolu.rel.ops._
import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.syntax.value.{BoolValueExpr, True, ValueExpr}

/**
 * A Relation is a set of tuples of a given arity. A Relation contains columns that store Unary relations. The number of columns is
 * the same as the arity. Order of columns is important.
 */
abstract class Relation extends Expression with Denestable{
  require(arity > 0, "A Relation must have a positive arity")

  def arity: Int

  override def projection = for (column <- columns) yield relationTableRef :@ column
  override def filter:Seq[BoolValueExpr] = Nil
  override def tables: Seq[TablePrim] = Nil

  final lazy val relationName: String = Relation.nextRelationName
  
  final def relationTableRef: TableRef = TableRef(relationName)

  def query: QueryExpr = querySpec

  def sqlExpr: HsqlDbExpr = query
  /** Returns all the columns of this relation */
  final def columns: Seq[String] = for(i <- 1 to arity) yield "col"+i
  
  final def -(r: Relation) = Difference(this, r)
  final def <:<(r: Relation) = Domain(this, r)
  final def ===(r: Relation) = Equality(this, r)
  final def &(r: Relation) = Intersection(this, r)
  final def ><(r: Relation) = Join(this, r)
  final def ++(r: Relation) = Override(this, r)
  final def ->(r: Relation) = Product(this, r)
  final def >:>(r: Relation) = Range(this, r)  
  final def in(r: Relation) = Subset(this, r)  
  final def +(r: Relation) = Union(this, r)
  final def unary_~ = Transpose(this)
}

object Relation {
  final def ^(r: Relation) = Closure(r)
  final def *(r: Relation) = ReflexClosure(r)
  
  final def some(r: Relation) = Some(r)
  final def no(r: Relation) = No(r)
  final def lone(r: Relation) = Lone(r)
  final def one(r: Relation) = One(r)
  private var ct = -1
  def nextRelationName(): String = {
    ct += 1
    "\"rel$%s\"" format ct 
  }
  
  def unapply(x: Expression): Option[Relation] = x match {
    case rel: Relation => Option(rel)
    case otherwise => None
  }
  
}