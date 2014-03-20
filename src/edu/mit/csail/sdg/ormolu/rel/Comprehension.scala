package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.data.access.Subquery

case class Comprehension(sub: Formula, args: Variable*) extends Relation {
  require(args forall {_.arity == 1}, "The arguements of a comprehension must have arity 1")

  override def arity: Int = args.size
  override def toString: String = args.map { v => v.name + ": " + v.relation }.mkString("{", ",", "\n\t| %s}").format(sub)

 override def query= querySpec

  override def projection = args flatMap (_.projection)
  override def filter = Vector(sub.boolExpr)
  override def tables = for (arg <- args) yield Subquery(arg.relation.query) as arg.name
}