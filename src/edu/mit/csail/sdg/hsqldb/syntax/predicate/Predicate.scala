package edu.mit.csail.sdg.hsqldb.syntax.predicate

import edu.mit.csail.sdg.hsqldb.syntax.value.{RowValueExpr, BoolValueExpr}

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 12:19 PM
 * To change this template use File | Settings | File Templates.
 */

trait Predicate extends BoolValueExpr with RowValueExpr