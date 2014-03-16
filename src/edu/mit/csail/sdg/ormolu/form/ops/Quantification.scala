package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.hsqldb.data.access.query.spec.DerivedColumn
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.SeqSelectSubList
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.syntax.literal.NumericLiteral
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Exists
import edu.mit.csail.sdg.hsqldb.syntax.value
import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.ormolu.rel.Comprehension
import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * A quantified Formula. Q (x:e)| F
 */
abstract class Quantification extends Formula {
  def sub: Formula
  def args: Seq[(String, Relation)]
}

/**
 * A quantified Formula. All (x:e)| F
 */
case class All(sub: Formula, args: (String, Relation)*) extends Quantification {
  override def toString: String = args.map { case (n, b) => n + ": " + n }.mkString("all {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = No(!sub, args: _*).boolExpr
}

/**
 * A quantified Formula. Some (x:e)| F
 */
case class Some(sub: Formula, args: (String, Relation)*) extends Quantification {
  override def toString: String = args.map { case (n, b) => n + ": " + n }.mkString("some {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = Exists(Subquery(Comprehension(sub, args: _*).query))
}

/**
 * A quantified Formula. No (x:e)| F
 */
case class No(sub: Formula, args: (String, Relation)*) extends Quantification {
  override def toString: String = args.map { case (n, b) => n + ": " + n }.mkString("no {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = value.Not(Exists(Subquery(Comprehension(sub, args: _*).query)))
}

/**
 * A quantified Formula. Lone (x:e)| F
 */
case class Lone(sub: Formula, args: (String, Relation)*) extends Quantification {
  override def toString: String = args.map { case (n, b) => n + ": " + n }.mkString("lone {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = {
    val queryExpr = Comprehension(sub, args: _*).query
    Comparison(
      Subquery(queryExpr.copy(selectList = SeqSelectSubList(DerivedColumn(value.CountAll) :: Nil))),
      Comparison.LessEq,
      value.SimpleRowValueExpr(NumericLiteral(1)))
  }
}

/**
 * A quantified Formula. One (x:e)| F
 */
case class One(sub: Formula, args: (String, Relation)*) extends Quantification {
  override def toString: String = args.map { case (n, b) => n + ": " + n }.mkString("one {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = {
    val queryExpr = Comprehension(sub, args: _*).query
    Comparison(
      Subquery(queryExpr.copy(selectList = SeqSelectSubList(DerivedColumn(value.CountAll) :: Nil))),
      Comparison.Equals,
      value.SimpleRowValueExpr(NumericLiteral(1)))
  }
}