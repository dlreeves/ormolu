package edu.mit.csail.sdg.monitor.observer

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/14/11
 * Time: 9:03 AM
 * To change this template use File | Settings | File Templates.
 */

case class Signature(name: String, id: String, remove: Option[Boolean] = None,
                     fields: Set[Field])