package edu.mit.csail.sdg.ormolu.rel
import edu.mit.csail.sdg.hsqldb.data.access.table.expression.TableExpr
import edu.mit.csail.sdg.hsqldb.data.access.Subquery
import edu.mit.csail.sdg.hsqldb.syntax.reference.TableRef
import edu.mit.csail.sdg.ormolu.form.Formula
import edu.mit.csail.sdg.hsqldb.data.access.query.spec._
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrimNoAs
import edu.mit.csail.sdg.hsqldb.data.access.table.primary.TablePrimWithAs

case class Comprehension(sub: Formula, args: (String, Relation)*) extends Relation {
  require(args forall {_._2.arity == 1}, "The arguements of a comprehension must have arity 1")

  override def arity: Int = args.size
  override def toString: String = args.map { case (n, b) => n + ": " + b }.mkString("{", ",", "\n\t| %s}").format(sub)

 override def query: QuerySpec = {

   val columnNames = for ((name, rel) <- args; col <- rel.columns) yield TableRef(name) :@ col
   val selectSubLists = for((columnName, asName) <- columnNames zip columns) yield DerivedColumn(columnName, Some(asName))
   val tablePrims = for ((name, rel) <- args) yield rel.tablePrim match {
     case table: TablePrimNoAs => table as name
     case TablePrimWithAs(table, _, _) => table as name
   }
   
   QuerySpec(SeqSelectSubList(selectSubLists), TableExpr(tablePrims))
  }
}