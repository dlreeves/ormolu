package edu.mit.csail.sdg.ormolu.rel.ops
import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Exists
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.syntax.value.{ Not, CountAll }
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.value.SimpleRowValueExpr
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.syntax.literal.NumericLiteral
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * A quantified expression. Q sub
 */
abstract class Qunatified extends Formula {
  def sub: Relation
}

/**
 * A quantified expression. Some sub means sub has some tuples
 */
case class Some(sub: Relation) extends Qunatified {
  override def toString: String = "some " + sub

  override def boolExpr = {

    Exists(Subquery(sub.query))
  }
}

/**
 * A quantified expression. Some sub means sub has no tuples
 */
case class No(sub: Relation) extends Qunatified {
  override def toString: String = "no " + sub

  override def boolExpr = Not(Exists(Subquery(sub.query)))
}

/**
 * A quantified expression. Some sub means sub has at most one tuple
 */
case class Lone(sub: Relation) extends Qunatified {
  override def toString: String = "lone " + sub

  override def boolExpr = {
    val queryExpr = sub.querySpec.copy(selectList = SeqSelectSubList(DerivedColumn(CountAll) :: Nil))

    Comparison(
      SimpleRowValueExpr(NumericLiteral(1)),
      Comparison.LessEq,
      Subquery(queryExpr)
      )
  }
}

/**
 * A quantified expression. Some sub means sub has exactly one tuple
 */
case class One(sub: Relation) extends Qunatified {
  override def toString: String = "one " + sub

  override def boolExpr = {
    val queryExpr = sub.querySpec.copy(selectList = SeqSelectSubList(DerivedColumn(CountAll) :: Nil))

    Comparison(
      SimpleRowValueExpr(NumericLiteral(1)),
      Comparison.Equals,
      Subquery(queryExpr)
      )
  }
}