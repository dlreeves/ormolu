package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.ormolu.rel.Relation
import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.data.access.table.Values
import edu.mit.csail.sdg.hsqldb.syntax.value.CaseSpec
import edu.mit.csail.sdg.hsqldb.data.access.Subquery


/**
 * A conditional SetExpr. cond => then else otherwise has value then when cond is true, else it has value otherwise
 */
case class ThenElse(cond: Formula, then: Relation, otherwise: Relation) extends Relation {
  require(then.arity == otherwise.arity,
    "The then and else expressions must have the same arity")

  override def arity: Int = then.arity
  override def toString: String = "if %s \n then %s \n else %s".format(cond, then, otherwise)

  override def query = Values(Vector(Vector(sqlExpr)))
  override def sqlExpr = {
    CaseSpec(searchWhens =(cond.boolExpr, Subquery(then.query)) :: Nil,
      elseClause = Option(Subquery(otherwise.query)))
  }
}

case class PartThenElse(cond: Formula, then: Relation) {
  def otherwise(r: Relation) = ThenElse(cond, then, r)
}