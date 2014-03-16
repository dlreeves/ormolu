package edu.mit.csail.sdg.monitor.observer.serialize.json

import net.liftweb.json.Serialization.{read => jsonRead, write => jsonWrite}
import net.liftweb.json.DefaultFormats
import java.text.SimpleDateFormat
import edu.mit.csail.sdg.monitor.observer._

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/14/11
 * Time: 2:29 PM
 * To change this template use File | Settings | File Templates.
 */

object JsonSerializer {
  var format = "yyyy-MM-dd HH:mm:ss"

  def write[T <: AnyRef](obj: T): String = {
    implicit val formats = DefaultFormats/*new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(format)
  }*/
    jsonWrite(obj)
  }

  implicit def readAsObservation(json: String) = {
    implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(format)
  }
    jsonRead[Observation](json)
  }

  implicit def readAsSignature(json: String) = {
    implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(format)
  }
    jsonRead[Signature](json)
  }

  implicit def readAsField(json: String) = {
    implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(format)
  }
    jsonRead[Field](json)
  }

  implicit def readaAsFieldValue(json: String) = {
    implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(format)
  }
    jsonRead[FieldValue](json)
  }

  implicit def readAsSigReference(json: String) = {
    implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(format)
  }
    jsonRead[SigReference](json)
  }
}