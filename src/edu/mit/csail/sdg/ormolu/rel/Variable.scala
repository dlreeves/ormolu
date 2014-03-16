package edu.mit.csail.sdg.ormolu.rel

case class Variable(name: String, override val arity: Int) extends Named(arity){
  override def toString: String = name
  def createTable = ""
  def tableRef = Univ.tableRef
}