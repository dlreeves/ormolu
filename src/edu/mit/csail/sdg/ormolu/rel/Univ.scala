package edu.mit.csail.sdg.ormolu.rel

/**
 * Univ is the AtomSet that contains all atoms. It represents the top of the base type system. 
 */
case object Univ extends AtomSet("univ") {
  val schema = "CREATE SCHEMA univ"

  val createTable = "CREATE TABLE %s (col1 INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY, atom VARCHAR(100) NOT NULL UNIQUE)" format tableRef.toSql

  override val createAddProcedure =
  "\nCREATE PROCEDURE \"univ\".add(IN atomString VARCHAR(100), OUT atom_id INTEGER)" +
  "\n\tMODIFIES SQL DATA" +
  "\n\tBEGIN ATOMIC" +
  "\n\t\tIF (atomString IN (SELECT atom FROM \"univ\".atoms)) THEN" +
  "\n\t\t\tSELECT col1 INTO atom_id FROM \"univ\".atoms WHERE atom = atomString;" +
  "\n\t\tELSE" +
  "\n\t\t\tINSERT INTO \"univ\".atoms VALUES (DEFAULT, atomString);" +
  "\n\t\t\tSET atom_id = IDENTITY();" +
  "\n\t\tEND IF;" +
  "\n\tEND"

  val createDelProcedure =
  "\nCREATE PROCEDURE \"univ\".del(IN atom VARCHAR(100))" +
  "\n\tMODIFIES SQL DATA" +
  "\n\tBEGIN ATOMIC" +
  "\n\t\tDELETE FROM \"univ\".atoms WHERE \"univ\".atoms.atom = atom;" +
  "\n\tEND"
}