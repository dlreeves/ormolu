package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.ormolu.rel.Relation
import edu.mit.csail.sdg.ormolu.rel.Iden

/**
 * The Reflexive-Transitive Closure (*) of a binary relation. *r is the smallest relation that contains r and is both
 * transitive and reflexive, and is obtained by adding the identity relation to the transitive closure
 */
case class ReflexClosure(relation: Relation) extends Relation {
  require(relation.arity == 2,
    "Transpose can only be used with a binary relation, r has arity " + relation.arity)

  override def arity: Int = relation.arity
  override def toString: String = "*" + relation

  override def query = (Closure(relation) + Iden).query
}