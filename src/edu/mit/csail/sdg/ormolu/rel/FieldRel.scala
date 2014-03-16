package edu.mit.csail.sdg.ormolu.rel

import edu.mit.csail.sdg.hsqldb.syntax.reference.SchemaRef


/**
 * A Field is a named relation that is derived from a field declaration in Alloy. Each Field has an owner,
 * which is the signature that it was declared in, and a subRelation, which is the relation declared in the field declaration.
 * When treated as a Relation its columns is the owner with the subRelation columns appended to the end.
 */
abstract class FieldRel(fieldName: String, owner: AtomSet, sub: Int) extends Named(sub + 1) {
  override val name: String = owner.name + "_" + fieldName

  override def toString: String = fieldName

  def tableRef = owner.tableRef.schema.head :@ fieldName
}

case class NonStateField(fieldName: String, owner: AtomSet, sub: Int) extends FieldRel(fieldName, owner, sub) {

  def createTable = {
    val table = "CREATE TABLE %s(%s, PRIMARY KEY(%s), FOREIGN KEY(col1) REFERENCES %s(col1) ON DELETE CASCADE)" +
      "\nCREATE TABLE %sAdd AS (TABLE %s) WITH NO DATA" +
      "\nCREATE TABLE %sDel AS (TABLE %s) WITH NO DATA" format (
      tableRef.toSql,
      (for (column <- columns) yield column + " INTEGER NOT NULL").mkString(", "),
      columns mkString ", ",
      owner.tableRef.toSql,
      tableRef.toSql,
      tableRef.toSql,
      tableRef.toSql,
      tableRef.toSql
      )
    table + createAddProcedure + createDelProcedure + createCommitProcedure
  }

  override def createAddProcedure = {
    val parameterList = for (i <- 1 to arity) yield "IN id%s INTEGER" format i
    val valuesList = for (i <- 1 to arity) yield "id%s" format i

    "\nCREATE PROCEDURE " + tableRef.toSql + "_add" + parameterList.mkString("(", ", ", ")") +
      "\n\tMODIFIES SQL DATA" +
      "\n\tBEGIN ATOMIC" +
      "\n\t\tIF (" + valuesList.mkString("(", ", ", ")") + "NOT IN (TABLE " + tableRef.toSql + ")) THEN" +
      "\n\t\t\tINSERT INTO " + tableRef.toSql + "Add VALUES" + valuesList.mkString("(", ", ", ")") + ";" +
      "\n\t\tEND IF;" +
    "\n\tEND"
  }

  def createDelProcedure = {
    val parameterList = for (i <- 1 to arity) yield "IN id%s INTEGER" format i
    val valuesList = for (i <- 1 to arity) yield "id%s" format i

    "\nCREATE PROCEDURE " + tableRef.toSql + "_del" + parameterList.mkString("(", ", ", ")") +
      "\n\tMODIFIES SQL DATA" +
      "\n\tBEGIN ATOMIC" +
      "\n\t\tIF (" + valuesList.mkString("(", ", ", ")") + "IN (TABLE " + tableRef.toSql + ")) THEN" +
      "\n\t\t\tINSERT INTO " + tableRef.toSql + "Del VALUES" + valuesList.mkString("(", ", ", ")") + ";" +
      "\n\t\tEND IF;" +
    "\n\tEND"
  }

  def createCommitProcedure = {
    val delCols = for (i <- 1 to arity) yield "col" + i

    "\nCREATE PROCEDURE " + tableRef.toSql + "_commit(IN use_merge BOOLEAN)" +
      "\n\tMODIFIES SQL DATA" +
      "\n\tBEGIN ATOMIC" +
      "\n\t\tIF(EXISTS((TABLE %sAdd) UNION (TABLE %sDel))) THEN".format(tableRef.toSql, tableRef.toSql) +
      "\n\t\t\tIF (use_merge) THEN" +
      "\n\t\t\t\tINSERT INTO %sDel (SELECT * FROM %s WHERE col1 IN (SELECT col1 FROM %sAdd));".format(tableRef.toSql, tableRef.toSql, tableRef.toSql) +
      "\n\t\t\tEND IF;" +
      "\n\t\t\tINSERT INTO %s (TABLE %sAdd);".format(tableRef.toSql, tableRef.toSql) +
      "\n\t\t\tDELETE FROM %s WHERE (%s) IN (TABLE %sDel);".format(tableRef.toSql, delCols.mkString(", "), tableRef.toSql) +
      "\n\t\t\tTRUNCATE TABLE %sAdd;".format(tableRef.toSql) +
      "\n\t\t\tTRUNCATE TABLE %sDel;".format(tableRef.toSql) +
      "\n\t\tEND IF;" +
      "\n\tEND"
  }
}

case class StateField(fieldName: String, owner: AtomSet, stateAtom: StateAtom) extends FieldRel(fieldName, owner, 1) {
  def createTable = {
    val table = "CREATE TABLE %s(col1 INTEGER NOT NULL, col2 INTEGER NOT NULL, PRIMARY KEY(col1, col2), FOREIGN KEY(col1) REFERENCES %s(col1) ON DELETE CASCADE, FOREIGN KEY(col2) REFERENCES %s(col1) ON DELETE CASCADE)" format (
      tableRef.toSql,
      owner.tableRef.toSql,
      stateAtom.tableRef.toSql
      )
    table + createCopyToTmpProcedure + createAddProcedure + createDelProcedure + createCommitProcedure
  }

  def createCopyToTmpProcedure = """
CREATE TABLE %sAdd AS (TABLE %s.current) WITH NO DATA
CREATE TABLE %sDel AS (TABLE %s.current) WITH NO DATA
""" format (
    tableRef.toSql, stateAtom.tableRef.schema.head.toSql, tableRef.toSql, stateAtom.tableRef.schema.head.toSql)

  override def createAddProcedure = {
    val parameterList = for (i <- 1 to stateAtom.stateArity) yield "IN id%s INTEGER" format i
    val valuesList = for (i <- 1 to stateAtom.stateArity) yield "id%s" format i

    "\nCREATE PROCEDURE " + tableRef.toSql + "_add" + parameterList.mkString("(", ", ", ")") +
      "\n\tMODIFIES SQL DATA" +
      "\n\tBEGIN ATOMIC" +
      "\n\t\tDECLARE atomState VARCHAR(100);" +
      "\n\t\tDECLARE idState INTEGER;" +
      "\n\t\tSET atomState = id1 || '$" + fieldName + "';" +
      "\n\t\tCALL " + Univ.tableRef.schema.head.toSql + ".add(atomState, idState);" +
      "\n\t\tIF ((id1, idState) NOT IN (TABLE " + tableRef.toSql + ")) THEN" +
      "\n\t\t\tINSERT INTO " + stateAtom.tableRef.toSql + " VALUES (idState);" +
      "\n\t\t\tINSERT INTO " + tableRef.toSql + " VALUES (id1, idState);" +
      "\n\t\tEND IF;" +
      ("\n\t\tIF ((idState, %s) NOT IN (TABLE %s.current)) THEN" format (valuesList.tail mkString ", ", stateAtom.tableRef.schema.head.toSql)) +
      ("\n\t\t\tINSERT INTO %sAdd VALUES(idState, %s);" format (tableRef.toSql, valuesList.tail mkString ", ")) +
      "\n\t\tEND IF;" +
      "\n\tEND"
  }

  def createDelProcedure = {
    val parameterList = for (i <- 1 to stateAtom.stateArity) yield "IN id%s INTEGER" format i
    val valuesList = for (i <- 1 to stateAtom.stateArity) yield "id" + i

    "\nCREATE PROCEDURE " + tableRef.toSql + "_del" + parameterList.mkString("(", ", ", ")") +
      "\n\tMODIFIES SQL DATA" +
      "\n\tBEGIN ATOMIC" +
      "\n\t\tDECLARE atomState VARCHAR(100);" +
      "\n\t\tDECLARE idState INTEGER;" +
      "\n\t\tSET atomState = id1 || '$" + fieldName + "';" +
      "\n\t\tCALL " + Univ.tableRef.schema.head.toSql + ".add(atomState, idState);" +
      ("\n\t\tIF ((idState, %s) IN (TABLE %s.current)) THEN" format (valuesList.tail mkString ", ", stateAtom.tableRef.schema.head.toSql)) +
      ("\n\t\t\tINSERT INTO %sDel VALUES(idState, %s);" format (tableRef.toSql, valuesList.tail mkString ", ")) +
      "\n\t\tEND IF;" +
      "\n\tEND"
  }

  def createCommitProcedure = {
    val stateAtomSchema = stateAtom.tableRef.schema.head.toSql
    val stateColumns = for (i <- 1 to stateAtom.stateArity) yield "col" + i

    "\nCREATE PROCEDURE " + tableRef.toSql + "_commit(IN use_merge BOOLEAN)" +
      "\n\tMODIFIES SQL DATA" +
      "\n\tBEGIN ATOMIC" +
      "\n\t\tIF(EXISTS((TABLE %sAdd) UNION (TABLE %sDel))) THEN".format(tableRef.toSql, tableRef.toSql) +
      "\n\t\t\tIF (use_merge) THEN" +
      "\n\t\t\t\tINSERT INTO %sDel (SELECT * FROM %s.current WHERE col1 IN (SELECT col1 FROM %sAdd));".format(tableRef.toSql, stateAtomSchema, tableRef.toSql) +
      "\n\t\t\tEND IF;" +
      "\n\t\t\tINSERT INTO %s.previous ((TABLE %s.current) EXCEPT (TABLE %s.previous));".format(stateAtomSchema, stateAtomSchema, stateAtomSchema) +
      "\n\t\t\tDELETE FROM %s.previous WHERE (%s) NOT IN (TABLE %s.current);".format(stateAtomSchema, stateColumns.mkString(", "), stateAtomSchema) +
      "\n\t\t\tINSERT INTO %s.past ((TABLE %s.previous) EXCEPT (TABLE %s.past));".format(stateAtomSchema, stateAtomSchema, stateAtomSchema) +
      "\n\t\t\tINSERT INTO %s.current (TABLE %sAdd);".format(stateAtomSchema, tableRef.toSql) +
      "\n\t\t\tDELETE FROM %s.current WHERE (%s) IN (TABLE %sDel);".format(stateAtomSchema, stateColumns.mkString(", "), tableRef.toSql) +
      "\n\t\t\tTRUNCATE TABLE %sAdd;".format(tableRef.toSql) +
      "\n\t\t\tTRUNCATE TABLE %sDel;".format(tableRef.toSql) +
      "\n\t\tEND IF;" +
      "\n\tEND"
  }
}