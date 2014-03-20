package edu.mit.csail.sdg.monitor.observer

import akka.actor.Actor
import java.sql.{CallableStatement, DriverManager}
import scala.collection.mutable.HashMap
import scala.xml.{XML, Node}
import edu.mit.csail.sdg.monitor.Monitor

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/14/11
 * Time: 6:23 PM
 * To change this template use File | Settings | File Templates.
 */

class Observer extends Actor {
  val cache = new HashMap[String, Int]
  def receive = {
    case Observation(_, sigs) => {
      val connection = DriverManager.getConnection("jdbc:apache:commons:dbcp:alloy")
      sigs foreach {
        handleSig(_, connection)
      }
      //println("CHECK " + sigs)
      Monitor.funcs foreach {f =>
        val stmt = connection.prepareCall("CALL BadArrival()")
        try{
        stmt.execute()
      val rs = stmt.getResultSet

      while (rs.next) {
        val validtime = for (col <- 1 to f.body.arity) yield rs.getString(col)
        println(f.name+": "+validtime.mkString("->"))
      }
        rs.close()

        } catch {
          case e:java.sql.SQLDataException =>  println(e.getSQLState)
        }finally{
        stmt.close()
        }
    }
      connection.close()
    }
  }

  def observe(obs: Observation) = {
    val connection = DriverManager.getConnection("jdbc:apache:commons:dbcp:alloy")
    obs.sigs foreach {
      handleSig(_, connection)
    }
    connection.close()
  }

  private def handleSig(sig: Signature, connection: java.sql.Connection) {
    //Add or Delete the
    if (sig.remove getOrElse false) {
      cache.remove(sig.id)
      val delCall = connection.prepareCall("CALL \"univ\".del(?)")
      delCall.setString(1, sig.id)
      delCall.executeUpdate()
      delCall.close()

    }
    else {
      val sigIdNumber = cache.getOrElseUpdate(sig.id, addSig(sig.name, sig.id, connection))

      sig.fields foreach {
        handleField(_, sig.name, sigIdNumber, connection)
      }
    }
  }

  private def handleField(field: Field, sigName: String, sigId: Int, connection: java.sql.Connection) {
    val arity = field.arity + 1 //Must add one for the sigId

    def addToCallBatch(value: Seq[SigReference]): Iterable[(Int, Int)] = {
      if (value.size == field.arity) {
        val sigIds: Seq[Int] = for (sigRef <- value) yield {
          //Make sure each sig is in the database first
          addSig(sigRef.name, sigRef.id, connection)
        }

        1 to arity zip sigId +: sigIds
      }
      else
        Nil
    }

    val fieldAdd: Set[FieldValue] = if (field.set.isDefined) Set(field.set.head) else field.add
    for (batch <- fieldAdd) {
      val params = addToCallBatch(batch.value)
      if (!params.isEmpty) {
        val addCall = connection.prepareCall("CALL \"%s\".%s_add(%s)" format (sigName, field.name, List.fill(arity)("?").mkString(", ")))
        params foreach {
          case (index, id) => addCall.setInt(index, id)
        }
        addCall.execute()
        addCall.close
      }
    }

    for (batch <- field.remove) {
      val params = addToCallBatch(batch.value)
      if (!params.isEmpty) {
        val removeCall = connection.prepareCall("CALL \"%s\".%s_del(%s)" format (sigName, field.name, List.fill(arity)("?").mkString(", ")))
        params foreach {
          case (index, id) => removeCall.setInt(index, id)
        }
        removeCall.execute()
        removeCall.close
      }
    }

    val commitCall = connection.prepareCall("CALL \"%s\".%s_commit(?)" format (sigName, field.name))
    commitCall.setBoolean(1, field.set.isDefined)
    commitCall.executeUpdate()
    commitCall.close()
  }

  private def addSig(name: String, id: String, connection: java.sql.Connection): Int = {
    val sigIdNumber = {
      val addToUniv = connection.prepareCall("CALL \"univ\".add(?, ?)")
      addToUniv.setString(1, id)
      addToUniv.registerOutParameter(2, java.sql.Types.INTEGER)
      addToUniv.execute()
      val res = addToUniv.getInt(2)
      addToUniv.close()
      res
    }
    val addToSig = connection.prepareCall("CALL \"%s\".add(?)" format name)
    addToSig.setInt(1, sigIdNumber)
    addToSig.execute()
    addToSig.close()

    sigIdNumber
  }
}







