package edu.mit.csail.sdg.ormolu.rel.ops

import edu.mit.csail.sdg.ormolu.rel.Relation


/**
 * The Product (->) of two relations. left -> right of two relations left and right is the relation you get by
 * taking every combination of a tuple from p and a tuple from q and concatenating them.
 */
case class Product(left: Relation, right: Relation) extends Relation {
  override def arity: Int = left.arity + right.arity
  override def toString: String = left + "->" + right

  override def query = querySpec

  override def projection = left.projection ++ right.projection
  override def filter = left.filter ++ right.filter
  override def tables = left.tables ++ right.tables
}