package edu.mit.csail.sdg.ormolu.form.ops

import edu.mit.csail.sdg.hsqldb.syntax.value
import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.ormolu.Predicate
import edu.mit.csail.sdg.ormolu.rel.{Variable, Comprehension, Relation}

/**
 * A quantified Formula. Q (x:e)| F
 */
abstract class Quantification extends Formula {
  def sub: Formula
  def args: Seq[Variable]
}

/**
 * A quantified Formula. All (x:e)| F
 */
case class All(sub: Formula, args: Variable*) extends Quantification {
  override def toString: String = args.map { v => v.name + ": " + v.relation }.mkString("all {", ",", "\n\t| %s}").format(sub)


  override def boolExpr = {
    value.Not(No(!sub, args: _*).boolExpr)
  }
}

/**
 * A quantified Formula. Some (x:e)| F
 */
case class Some(sub: Formula, args: Variable*) extends Quantification {
  override def toString: String = args.map { v => v.name + ": " + v.relation }.mkString("some {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = Relation.some(Comprehension(sub, args: _*)).boolExpr
}

/**
 * A quantified Formula. No (x:e)| F
 */
case class No(sub: Formula, args: Variable*) extends Quantification {
  override def toString: String = args.map { v => v.name + ": " + v.relation }.mkString("no {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = Relation.no(Comprehension(sub, args: _*)).boolExpr
}

/**
 * A quantified Formula. Lone (x:e)| F
 */
case class Lone(sub: Formula, args: Variable*) extends Quantification {
  override def toString: String = args.map { v => v.name + ": " + v.relation }.mkString("lone {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = Relation.lone(Comprehension(sub, args: _*)).boolExpr
}

/**
 * A quantified Formula. One (x:e)| F
 */
case class One(sub: Formula, args: Variable*) extends Quantification {
  override def toString: String = args.map { v => v.name + ": " + v.relation }.mkString("one {", ",", "\n\t| %s}").format(sub)

  override def boolExpr = Relation.one(Comprehension(sub, args: _*)).boolExpr
}