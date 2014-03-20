package edu.mit.csail.sdg.alloy4.translator

import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil
object Translator extends App{
  val m = CompUtil.parseEverything_fromFile(null, null, """C:\Users\Dwayne\Desktop\Work\alloy\flightdataVersioned.als""")
  //Make Tables For Sigs
  val model = new OrmoluModel(m)
//  println(model.facts.mkString("\n"))
//  println(model.funcs.mkString("\n"))
//  println(model.fields.mkString("\n"))
//  println(model.sigs.mkString("\n"))
//  model.predicates foreach {x => println(x.definition)}
  model.functions foreach {x => println(x.definition)}
  println("Done")
}