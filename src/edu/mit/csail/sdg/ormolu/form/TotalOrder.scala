package edu.mit.csail.sdg.ormolu.form

import edu.mit.csail.sdg.ormolu.rel.Relation

abstract class TotalOrder(elem: Relation, first: Relation, next: Relation) extends Formula {
  //def boolExpr = BoolExpr.True
}