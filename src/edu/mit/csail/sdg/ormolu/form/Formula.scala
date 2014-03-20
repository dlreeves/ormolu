package edu.mit.csail.sdg.ormolu.form

import edu.mit.csail.sdg.ormolu.form.ops._
import edu.mit.csail.sdg.hsqldb.syntax.value.BoolValueExpr
import edu.mit.csail.sdg.hsqldb.syntax.value
import edu.mit.csail.sdg.ormolu.rel.{Variable, Relation}
import edu.mit.csail.sdg.ormolu.{Predicate, Expression}
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.{QuerySpec, Star}
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.data.access.table.Values

/**
 * A Formula is an Expression that when evaluated would yield a boolean value, true or false.
 */
abstract class Formula extends Expression {

  def boolExpr: BoolValueExpr = value.True
  override def sqlExpr = boolExpr

  lazy val formName = Formula.nextFormulaName()

  final def and(f: Formula) = And(this, f)

  final def <=>(f: Formula) = Iff(this, f)

  final def ==>(f: Formula) = Implies(this, f)

  final def or(f: Formula) = Or(this, f)
  
  final def unary_! = Not(this)
  
  //final def then(r: Relation) = PartThenElse(this, r)
  final def then(f: Formula) = Implies(this, f)

  def querySpec = {
    val tablePrim = Subquery(Values(Vector(Vector(value.True), Vector(value.False)))).as(formName, "col1" :: Nil)
    val comparison = Comparison((TableRef(formName) :@ "col1"), Comparison.Equals, boolExpr)

    QuerySpec(Star, TableExpr(tablePrim::Nil, Option(comparison)))
  }
}

object Formula {
  private var ct = -1
  def nextFormulaName(): String = {
    ct += 1
    "\"formula$%s\"" format ct
  }
  def unapply(x: Expression): Option[Formula] = x match {
    case form: Formula => Option(form)
    case otherwise => None
  }
  
  final def all(args: Variable*)(f: Formula) = All(f, args:_*)
  final def some(args: Variable*)(f: Formula) = Some(f, args:_*)
  final def no(args: Variable*)(f: Formula) = No(f, args:_*)
  final def lone(args: Variable*)(f: Formula) =  Lone(f, args:_*)
  final def one(args: Variable*)(f: Formula) =  One(f, args:_*)
}