package edu.mit.csail.sdg.ormolu

import edu.mit.csail.sdg.ormolu.rel.Relation

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/4/11
 * Time: 10:41 AM
 * To change this template use File | Settings | File Templates.
 */

object Util {
  def newTmpName: String = ""

/*  def toFromItem(relation: Relation, tmpName: String) = relation match {
    case named: Named => TableItem(named.toTableName)
    case otherwise => DerivedTableItem(TableSubquery(relation.query), Some(tmpName))
  }

  def toColumnNames(relation: Relation, tmpName: String) = {
    val name = relation match {
      case named: Named => named.name
      case otherwise => tmpName
    }

    relation.columns map {ColumnName(Some(name), _:String)}
  }*/
}