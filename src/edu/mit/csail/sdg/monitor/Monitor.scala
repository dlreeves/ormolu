package edu.mit.csail.sdg.monitor


import org.apache.commons.dbcp.PoolingDriver
import org.apache.commons.dbcp.PoolableConnectionFactory
import org.apache.commons.dbcp.DriverManagerConnectionFactory
import java.sql.DriverManager
import org.apache.commons.pool.impl.GenericObjectPool
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil
import edu.mit.csail.sdg.monitor.observer.Observer
import scala.io.Source
import edu.mit.csail.sdg.monitor.observer.serialize.json.JsonSerializer._
import akka.actor.{PoisonPill, Actor}
import edu.mit.csail.sdg.alloy4.translator.{AlloyToOrmolu, OrmoluModel}
import edu.mit.csail.sdg.ormolu.CheckedFunction


/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/15/11
 * Time: 1:14 PM
 * To change this template use File | Settings | File Templates.
 */

object Monitor {
  var funcs: Iterable[CheckedFunction] = Nil
  def main(args: Array[String]) = {
    setupConnectionPool()
    startUpDb()

    val observer = Actor.actorOf[Observer]
    observer.start()
    Source.fromFile("observations.json").getLines().foreach{ line =>
     observer ! readAsObservation(line)
   }
    observer !! PoisonPill

   /* val observer = new Observer
    Source.fromFile("observations.json").getLines().foreach{ line =>
     observer observe readAsObservation(line)
   }*/
  }

  private def setupConnectionPool() {
    val connectionPool = new GenericObjectPool(null)
    val connectionFactory = new DriverManagerConnectionFactory("jdbc:hsqldb:mem:alloy", null)
    val poolableConnectionFactory = new PoolableConnectionFactory(connectionFactory, connectionPool, null, null, false, true)


    Class.forName("org.apache.commons.dbcp.PoolingDriver")
    val driver = DriverManager.getDriver("jdbc:apache:commons:dbcp:").asInstanceOf[PoolingDriver]
    driver.registerPool("alloy", connectionPool)
  }

  private def startUpDb() {
    val c = DriverManager.getConnection("jdbc:apache:commons:dbcp:alloy")
    val s = c.createStatement()
    val m = CompUtil.parseEverything_fromFile(null, null, """C:\Users\Dwayne\Desktop\Work\alloy\flightdataVersioned.als""")
    //Make Tables For Sigs
    val model = new OrmoluModel(m)
    model.sigTables ++ model.functions.map(_.definition)foreach {
      t => println(t)
        s addBatch t
    }
    funcs = model.functions
    s.executeBatch()
    s.close()
    c.close()
  }
}