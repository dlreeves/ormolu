package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.syntax.reference.SchemaRef

/**
 * An AtomSet represents a signature, a named set of Atoms in the Alloy language.
 */
abstract class AtomSet(val name: String) extends Named(1) {
  override def toString: String = name

  override val tableRef = SchemaRef('"' + name.replaceFirst("this/", "") + '"') :@ "atoms"
}

case class StateAtom(override val name: String) extends AtomSet(name) {
  def createTable = {
    "CREATE TABLE %s (col1 INTEGER PRIMARY KEY, FOREIGN KEY(col1) REFERENCES %s(col1) ON DELETE CASCADE)" format (tableRef.toSql, Univ.tableRef.toSql)
  }

  def stateArity = if (name.contains("Unary")) 2 else 3
}

case class PrimAtom(override val name: String, parent: AtomSet) extends AtomSet(name) {
  def createTable = {
    "CREATE TABLE %s (col1 INTEGER PRIMARY KEY, FOREIGN KEY(col1) REFERENCES %s(col1) ON DELETE CASCADE)" format (tableRef.toSql, parent.tableRef.toSql)
  }

  override val createAddProcedure = {
     val callParent = if (parent == Univ) "" else "\n\t\tCALL " + parent.tableRef.schema.head.toSql + ".add(id);"
    "\nCREATE PROCEDURE " + tableRef.schema.head.toSql + ".add(id INTEGER)" +
    "\n\tMODIFIES SQL DATA" +
    "\n\tBEGIN ATOMIC" +
    callParent +
    "\n\t\tIF (id NOT IN (TABLE " + tableRef.toSql + ")) THEN" +
    "\n\t\t\tINSERT INTO " + tableRef.toSql + " VALUES(id);" +
    "\n\t\tEND IF;" +
    "\n\tEND"
  }
}

case class SubSetAtom(override val name: String, parents: AtomSet*) extends AtomSet(name) {
  def createTable = {
    "CREATE VIEW %s (col1) AS %s" format (tableRef.toSql,
      if (parents.size < 2) parents.head.query.toSql
      else {
        val unionParents = parents reduceLeft ((_: Relation) + _)
        unionParents.query.toSql
      })
  }
}