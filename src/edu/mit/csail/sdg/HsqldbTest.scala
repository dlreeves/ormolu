package edu.mit.csail.sdg

import java.sql.DriverManager
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil
import edu.mit.csail.sdg.alloy4.translator.OrmoluModel

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/6/11
 * Time: 1:53 PM
 * To change this template use File | Settings | File Templates.
 */

object HsqldbTest extends App{
  val c = DriverManager.getConnection("jdbc:hsqldb:mem:alloy")
  val s = c.createStatement()
  val m = CompUtil.parseEverything_fromFile(null, null, """C:\Users\Dwayne\Desktop\Work\alloy\flightdataVersioned.als""")
  //Make Tables For Sigs
  val model = new OrmoluModel(m)
  model.sigTables foreach {t => println(t) 
  s execute t}
  s.close()
  c.close()
}