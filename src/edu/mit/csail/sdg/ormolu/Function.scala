package edu.mit.csail.sdg.ormolu

import edu.mit.csail.sdg.alloy4compiler.ast.Func
import edu.mit.csail.sdg.alloy4.translator.AlloyToOrmolu
import edu.mit.csail.sdg.ormolu.rel.{Univ, Relation}
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.hsqldb.data.access.query.spec.{SeqSelectSubList, DerivedColumn, QuerySpec}
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.{TablePrim, TablePrimWithAs}
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.joined.JoinOn
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison
import edu.mit.csail.sdg.hsqldb.syntax.predicate.Comparison.Equals
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.Subquery

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/18/11
 * Time: 11:29 PM
 * To change this template use File | Settings | File Templates.
 */

case class CheckedFunction(func: Func) {
  require(func.params().isEmpty)
  require(!func.isPred)

  val name = func.label.replaceFirst("this/", "")
  val body = (for (Relation(rel) <- AlloyToOrmolu.visitThis(func.getBody))
  yield rel).head

  val returnType = {
    "TABLE(%s)".format(body.columns.mkString("", " VARCHAR(100), ", " VARCHAR(100)"))
  }
  val bodyName = "\""+name+"$ans\""
  val query = {
    //val atoms = for (col <- 1 to body.arity) yield Univ.tableRef as "atom"+col
    val derivedColumns = for (col <- 1 to body.arity) yield DerivedColumn(TableRef("atom"+col) :@ "atom")
    val tablePrim = (1 to body.arity).foldLeft(Subquery(body.query).as(bodyName, body.columns):TablePrim){(tab, col) =>
      val univ = TableRef("atom"+col)
      JoinOn(tab, Univ.tableRef as "atom"+col, Comparison(TableRef(bodyName) :@ body.columns(col - 1), Equals, univ :@ "col1"))
    }
    QuerySpec(SeqSelectSubList(derivedColumns), TableExpr(tablePrim :: Nil)).toSql
  }

  def call = "CALL %s()" format name

  def definition =
  "CREATE FUNCTION " + name + "()"+
  "\n\tRETURNS "+returnType+
  "\n\tREADS SQL DATA"+
  "\n\tRETURN TABLE (%s)" format query



}