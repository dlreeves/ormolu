package edu.mit.csail.sdg.monitor.observer

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/14/11
 * Time: 9:08 AM
 * To change this template use File | Settings | File Templates.
 */

case class Field(name: String, arity: Int, add: Set[FieldValue], remove: Set[FieldValue], set: Option[FieldValue]) {

}