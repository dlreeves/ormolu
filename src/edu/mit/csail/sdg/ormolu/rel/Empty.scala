package edu.mit.csail.sdg.ormolu.rel


/**
 * Empty is the AtomSet with no elements. It represents the bottom of the base type system. 
 */
case object Empty extends AtomSet("nothing") {
  val createTable = "CREATE TABLE %s (col1 INTEGER PRIMARY KEY)" format tableRef.toSql
}