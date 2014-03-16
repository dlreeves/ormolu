package edu.mit.csail.sdg

import scala.xml.{Node, XML}
import java.sql.{Timestamp, DriverManager}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import java.io.FileWriter


/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/14/11
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */
/**
 * Used to convert the contents of the derby database 20110104_SeqSched/lowrate to a text file
 * of observations using the JSON format. Used so that a connection to a derby database isn't
 * needed when testing the monitor
 */
object SampleDatabaseToJson extends App {
  //Connect to the derby database
  val db: String = "20110104_SeqSched/lowrate"
  val conn = DriverManager.getConnection("jdbc:derby:" + db)

  //Select the entries from the database to convert into json
  val stmt = conn.createStatement
  stmt.execute("SELECT validtime, data FROM tfdmfeaturecollection ORDER BY validtime")
  val rs = stmt.getResultSet()
  val fileWriter = new FileWriter("observations.json")
  while (rs.next) {
    val validtime = rs.getTimestamp("validtime")
    val data = rs.getClob("data")

    val xml: Node = XML.load(data.getCharacterStream)
    data.free()
    val json = convert(validtime, xml)
    if (!json.isEmpty) fileWriter.write(json + "\n")
  }
  fileWriter.close()
  rs.close()
  stmt.close()
  conn.close()

  def convert(timestamp: Timestamp, xml: Node) = {
    val sigs = xml.child flatMap {feature =>
      feature.label match {
        case "ArrivalProgressFeature" => Some(handleArrivalProgressFeature(feature))
        case "DefaultFlightStripFeature" => None
        case "DepartureProgressFeature" => Some(handleDepartureProgressFeature(feature))
        case "FlightDisplayDataFeature" => Some(handleFlightDisplayDataFeature(feature))
        case "FlightModelFeature" => None
        case "NasUpdateFeature" => Some(handleNasUpdateFeature(feature))
        case "RunwayAssignmentFeature" => None
        case "SequenceSchedulingFeature" => None
        case "TargetCorrelationFeature" => None
        case "TargetDropFeature" => None
        case "TargetMergeFeature" => None
        case "TaxiRoutingFeature" => None
        case "#PCDATA" => None
        case otherwise => println(otherwise); println(xml); throw new RuntimeException()
      }
    }
    if (sigs.isEmpty)
      ""
    else
      compact(render(("time" -> timestamp.toString) ~ ("sigs" -> sigs)))

  }

  def handleArrivalProgressFeature(xml: Node): JObject = {
    val aircraftId: String = (xml \ "flightIdentifiers" \ "acid").text

    val arrive = ("name" -> "Arrive") ~ ("id" -> "Arrive")

    val intent =
      ("name" -> "intent") ~
      ("arity" -> 1) ~
      ("set" -> ("value" -> List(arrive)))

    ("name" -> "Aircraft") ~
    ("id" -> aircraftId) ~
    ("fields" -> List(intent))
  }

  def handleDepartureProgressFeature(xml: Node): JObject = {
    val aircraftId: String = (xml \ "flightIdentifiers" \ "acid").text

    val depart = ("name" -> "Depart") ~ ("id" -> "Depart")

    val intent =
      ("name" -> "intent") ~
      ("arity" -> 1) ~
      ("set" -> ("value" -> List(depart)))

    ("name" -> "Aircraft") ~
    ("id" -> aircraftId) ~
    ("fields" -> List(intent))
  }

  def handleFlightDisplayDataFeature(xml: Node): JObject = {
    val aircraftId: String = (xml \ "flightDisplayData" \ "acid").text

    val aircraft = ("name" -> "Aircraft") ~ ("id" -> aircraftId)

    val value = ("value" -> List(aircraft))

    val flightData =
      ("name" -> "flightData") ~
      ("arity" -> 1) ~
      ("add" -> List(value))

    ("name" -> "TFDM") ~
    ("id" -> "TFDM") ~
    ("fields" -> List(flightData))
  }

  def handleNasUpdateFeature(xml: Node): JObject = {
    val aircraftId: String = (xml \ "flightIdentifiers" \ "acid").text

    val aircraft = ("name" -> "Aircraft") ~ ("id" -> aircraftId)

    val value = ("value" -> List(aircraft))

    val flightData =
      ("name" -> "flightData") ~
      ("arity" -> 1) ~
      ("add" -> List(value))

    ("name" -> "TFDM") ~
    ("id" -> "TFDM") ~
    ("fields" -> List(flightData))
  }
}