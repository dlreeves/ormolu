package edu.mit.csail.sdg.ormolu.form

import edu.mit.csail.sdg.ormolu.Expression
import edu.mit.csail.sdg.ormolu.rel.Relation
import edu.mit.csail.sdg.ormolu.form.ops._
import edu.mit.csail.sdg.hsqldb.syntax.value.BoolValueExpr
import edu.mit.csail.sdg.hsqldb.syntax.value

/**
 * A Formula is an Expression that when evaluated would yield a boolean value, true or false.
 */
abstract class Formula extends Expression {

  def boolExpr: BoolValueExpr = value.True
  override def sqlExpr = boolExpr

  final def and(f: Formula) = And(this, f)

  final def <=>(f: Formula) = Iff(this, f)

  final def ==>(f: Formula) = Implies(this, f)

  final def or(f: Formula) = Or(this, f)
  
  final def unary_! = Not(this)
  
  final def then(r: Relation) = PartThenElse(this, r)
  final def then(f: Formula) = Implies(this, f)
}

object Formula {
  
  def unapply(x: Expression): Option[Formula] = x match {
    case form: Formula => Option(form)
    case otherwise => None
  }
  
  final def all(args: (String, Relation)*)(f: Formula) = All(f, args:_*)
  final def some(args: (String, Relation)*)(f: Formula) = Some(f, args:_*)
  final def no(args: (String, Relation)*)(f: Formula) = No(f, args:_*)
  final def lone(args: (String, Relation)*)(f: Formula) =  Lone(f, args:_*)
  final def one(args: (String, Relation)*)(f: Formula) =  One(f, args:_*)
}