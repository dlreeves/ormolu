package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.ormolu.Expression
import edu.mit.csail.sdg.hsqldb.data.access.QueryExpr
import edu.mit.csail.sdg.hsqldb.HsqlDbExpr
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.syntax.value.ValueExpr
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrim
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.ormolu.rel.ops._

/**
 * A Relation is a set of tuples of a given arity. A Relation contains columns that store Unary relations. The number of columns is
 * the same as the arity. Order of columns is important.
 */
abstract class Relation extends Expression{
  require(arity > 0, "A Relation must have a positive arity")

  def arity: Int

  lazy val relationName: String = Relation.nextRelationName
  
  def relationTableRef: TableRef = TableRef(relationName)

  def query: QueryExpr = Univ.query
  
  def tableColumns: Seq[ValueExpr] = for (column <- columns) yield relationTableRef :@ column
  
  def tablePrim: TablePrim = Subquery(query) as relationName

  def sqlExpr: HsqlDbExpr = query
  /** Returns all the columns of this relation */
  final val columns: Seq[String] = for(i <- 1 to arity) yield "col"+i
    
  /** Returns the first column of this relation */
  final def first: String = columns(0)
  
  /** Returns all but the first column of this relation */
  final def butFirst: Seq[String] = columns.tail
  
  /** Returns the last column of this relation */
  final def last: String = columns(arity - 1)
  
  /** Returns all but the last column of this relation */
  final def butLast: Seq[String] = columns.init;

  
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