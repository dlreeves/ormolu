package edu.mit.csail.sdg.ormolu

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr

/**
 * An Expression is anything that can be translated to another form for evaluation.
 */
abstract class Expression{
  def sqlExpr: HsqlDbExpr
}