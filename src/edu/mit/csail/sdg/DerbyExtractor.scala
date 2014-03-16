package edu.mit.csail.sdg

import edu.mit.csail.sdg.monitor.observer.Observation
import scala.io.Source
import edu.mit.csail.sdg.monitor.observer.serialize.json.JsonSerializer._


/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/13/11
 * Time: 8:56 PM
 * To change this template use File | Settings | File Templates.
 */

object DerbyExtractor extends App{
   Source.fromFile("observations.json").getLines().foreach{ line =>
     //println(line)
     println(write(readAsObservation(line)))
   }
}