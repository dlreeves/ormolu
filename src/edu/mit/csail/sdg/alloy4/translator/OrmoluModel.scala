package edu.mit.csail.sdg.alloy4.translator

import edu.mit.csail.sdg.alloy4compiler.ast.Module
 import scala.collection.JavaConversions._
import edu.mit.csail.sdg.ormolu.rel.{Iden, StateAtom, Univ}

class OrmoluModel(module: Module) {
  var atomDataType = "VARCHAR(32)"
  def domain = "CREATE DOMAIN AtomString AS %s" format atomDataType

  val funcs = for(m <- module.getAllReachableModules; fun <- m.getAllFunc; f <- AlloyToOrmolu.visitThis(fun.getBody)) yield {
    fun.label -> f
  }
  val facts = for(m <- module.getAllReachableModules; fact <- m.getAllFacts; f <- AlloyToOrmolu.visitThis(fact.b)) yield {
    fact.a -> f
  }
  val univTable: String = "CREATE SCHEMA %s \n%s %s %s;".format (
    Univ.tableRef.schema.head.toSql,
    Univ.createTable,
    Univ.createAddProcedure,
    Univ.createDelProcedure)
  val idenTable: String = Iden.createTable
  val stateTables: Seq[String] = for{sig <- module.getAllReachableSigs if sig.isPrivate == null
                      s   <- AlloyToOrmolu.visit(sig) if s.isInstanceOf[StateAtom]
                      } yield "CREATE SCHEMA %s \n%s %s;".format (s.tableRef.schema.head.toSql, s.createTable,
      (for {field <- sig.getFields if field.isPrivate == null
            f     <- AlloyToOrmolu.visit(field)} yield f.createTable).mkString("\n", "\n", ""))
  val nonStateTables: Seq[String] = for{sig <- module.getAllReachableSigs if sig.isPrivate == null
                      s   <- AlloyToOrmolu.visit(sig) if !s.isInstanceOf[StateAtom] && s != Univ
                      } yield "CREATE SCHEMA %s \n%s %s %s;".format (s.tableRef.schema.head.toSql, s.createTable,
      (for {field <- sig.getFields if field.isPrivate == null
            f     <- AlloyToOrmolu.visit(field)} yield f.createTable).mkString("\n", "\n", ""),
     s.createAddProcedure)

  val sigTables: Seq[String] = univTable +: idenTable +: stateTables ++: nonStateTables

}