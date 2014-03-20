package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.ormolu.rel.Relation


/**
 * The Transpose (~) of a binary relation. ~r takes the mirror image of r, forming a new relation by reversing the order of atoms in each tuple.
 */
case class Transpose(relation: Relation) extends Relation {
  require(relation.arity == 2,
    "Transpose can only be used with a binary relation, r has arity " + relation.arity)

  override def arity: Int = relation.arity
  override def toString: String = "~" + relation

  override def query = querySpec

  override def projection = relation.projection.reverse
  override def filter = relation.filter
  override def tables = relation.tables
}