package edu.mit.csail.sdg.hsqldb.data.access.query.spec

import edu.mit.csail.sdg.hsqldb.HsqlDbExpr

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:56 PM
 * To change this template use File | Settings | File Templates.
 */

trait SelectList extends HsqlDbExpr{
  def subLists : Seq[SelectSubList]
}

case object Star extends SelectList{ val toSql = "*"; val subLists = Nil}

case class SeqSelectSubList(subLists: Seq[SelectSubList]) extends SelectList{
  def toSql = subLists.map(_.toSql) mkString ", "
}