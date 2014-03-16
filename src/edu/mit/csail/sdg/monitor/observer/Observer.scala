package edu.mit.csail.sdg.monitor.observer

import akka.actor.Actor
import java.sql.{CallableStatement, DriverManager}
import scala.collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/14/11
 * Time: 6:23 PM
 * To change this template use File | Settings | File Templates.
 */

class Observer /*extends Actor*/ {
  val cache = new HashMap[String, Int]
  /*def receive = {
    case Observation(_, sigs) => {
      val connection = DriverManager.getConnection("jdbc:apache:commons:dbcp:alloy")
      sigs foreach {
        handleSig(_, connection)
      }
      connection.close()
    }
  }*/

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
      val delCall = connection.prepareCall("CALL \"univ\".del(?)")
      delCall.setString(1, sig.id)
      println("CALL \"univ\".del(%s)" format sig.id)
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
    /*def addToCallBatch(value: Seq[SigReference]) : Option[CallableStatement => Unit] = {
      if (value.size == field.arity) {
        val sigIds: Seq[Int] = for (sigRef <- value) yield {
          //Make sure each sig is in the database first
          cache.getOrElseUpdate(sigRef.id, addSig(sigRef.name, sigRef.id, connection))
        }

        Some(
          (call: CallableStatement) => {
        println("CALL \"%s\".%s_add(%s)" format (sigName, field.name, sigId +: sigIds mkString ", "))
        1 to arity zip sigId +: sigIds foreach { case (index, id) => call.setInt(index, id) }
        call.addBatch()
        }
        )
      }
      None
    }*/
    def addToCallBatch(value: Seq[SigReference]): Iterable[(Int, Int)] = {

      val sigIds: Seq[Int] = for (sigRef <- value) yield {
        //Make sure each sig is in the database first
        addSig(sigRef.name, sigRef.id, connection)
      }


      println("CALL \"%s\".%s_add(%s)" format (sigName, field.name, sigId +: sigIds mkString ", "))
      1 to arity zip sigId +: sigIds
    }

    val fieldAdd: Set[FieldValue] = if (field.set.isDefined) Set(field.set.head) else field.add

    for (batch <- fieldAdd) {


      val s = addToCallBatch(batch.value)
      println(s)
      if (!s.isEmpty){
        val addCall = connection.prepareCall("CALL \"%s\".%s_add(%s)" format (sigName, field.name, List.fill(arity)("?").mkString(", ")))
        s foreach { case (index, id) => addCall.setInt(index, id) }
        addCall.execute()
        addCall.close
      }
    }



    /*for (batch <- field.remove) {


            val s = addToCallBatch(batch.value)
            println (s)
            if (!s.isEmpty){
              val removeCall = connection.prepareCall("CALL \"%s\".%s_del(%s)" format (sigName, field.name, List.fill(arity)("?").mkString(", ")))
              s foreach { case (index, id) => removeCall.setInt(index, id) }
              removeCall.execute()

              removeCall.close
            }
          }
    */

    /*val addCalls = fieldAdd foreach {fv =>
      addToCallBatch(fv.value)}
    val removeCalls = field.remove flatMap {fv => addToCallBatch(fv.value)}


    if(!addCalls.isEmpty) {
      val addCall = connection.prepareCall("CALL \"%s\".%s_add(%s)" format (sigName, field.name, List.fill(arity)("?").mkString(", ")))
      for (batch <- addCalls) { batch(addCall)}
      addCall.executeBatch()
      addCall.close
    }

    if(!removeCalls.isEmpty) {
      val removeCall = connection.prepareCall("CALL \"%s\".%s_del(%s)" format (sigName, field.name, List.fill(arity)("?").mkString(", ")))
      for (batch <- removeCalls) { batch(removeCall)}
      removeCall.executeBatch()
      removeCall.close
    }*/

    val commitCall = connection.prepareCall("CALL \"%s\".%s_commit(?)" format (sigName, field.name))
    println("CALL \"%s\".%s_commit(%s)" format (sigName, field.name, field.set.isDefined))
    commitCall.setBoolean(1, field.set.isDefined)
    commitCall.executeUpdate()
    commitCall.close()
  }

  private def addSig(name: String, id: String, connection: java.sql.Connection): Int = {
    val sigIdNumber = {
      val addToUniv = connection.prepareCall("CALL \"univ\".add(?, ?)")
      println("ADDSIG CALL \"univ\".add(%s, ?)" format (id))
      addToUniv.setString(1, id)
      addToUniv.registerOutParameter(2, java.sql.Types.INTEGER)
      addToUniv.execute()
      val res = addToUniv.getInt(2)
      addToUniv.close()
      res
    }
    val addToSig = connection.prepareCall("CALL \"%s\".add(?)" format name)
    println("ADDSIG CALL \"%s\".add(%s)" format (name, sigIdNumber))
    addToSig.setInt(1, sigIdNumber)
    addToSig.execute()
    addToSig.close()
    println("type: %s, name: %s, id:%s" format (name, id, sigIdNumber))
    sigIdNumber

  }
}







