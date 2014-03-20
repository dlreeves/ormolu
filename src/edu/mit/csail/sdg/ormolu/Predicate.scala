package edu.mit.csail.sdg.ormolu

import edu.mit.csail.sdg.alloy4compiler.ast.Func
import scala.collection.JavaConversions._
import edu.mit.csail.sdg.alloy4.translator.AlloyToOrmolu
import edu.mit.csail.sdg.ormolu.rel.{Relation, Variable}
import edu.mit.csail.sdg.hsqldb.syntax.value.{BoolValueExpr, ValueExpr}
import edu.mit.csail.sdg.hsqldb.syntax.literal.{FunctionLiteral}

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/16/11
 * Time: 9:24 PM
 * To change this template use File | Settings | File Templates.
 */

case class Predicate(name: String, variables: Seq[Variable], body: Expression) {
  def this(func: Func) = this(func.label.replaceFirst("this/", ""),
    for (param <- func.params(); Relation(rel) <- AlloyToOrmolu.visitThis(param.`type`.toExpr)) yield Variable(param.label, rel),
    AlloyToOrmolu.visitThis(func.getBody).head)

  val parameters = for (variable <- variables; parameter <- variable.parameters) yield parameter
  def call(args: Seq[ValueExpr]): BoolValueExpr =  new FunctionLiteral(name + (args.map(_.toSql).mkString("(", ", ", ")"))) with BoolValueExpr
  def definition =
    "CREATE FUNCTION " + name + parameters.mkString("(", " INTEGER, ", " INTEGER)") +
    "\n\tRETURNS BOOLEAN"+
    "\n\tREADS SQL DATA"+
    "\n\tBEGIN ATOMIC"+
    variables.map("\n\t\t" +_.createTable).mkString +
    variables.map("\n\t\t" +_.insertValues).mkString +
    "\n\t\tRETURN " + body.sqlExpr.toSql + ";" +
    "\n\tEND"

}
