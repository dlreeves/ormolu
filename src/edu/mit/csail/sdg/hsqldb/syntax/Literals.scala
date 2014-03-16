package edu.mit.csail.sdg.hsqldb.syntax

import java.lang.Boolean
import edu.mit.csail.sdg.hsqldb.syntax.value.{NonParenValueExpr, BoolValueExpr}

/**
 * Created by IntelliJ IDEA.
 * User: Dwayne
 * Date: 7/8/11
 * Time: 10:17 AM
 * To change this template use File | Settings | File Templates.
 */

/**
 * A literal in HSQLDB
 */

object literal {
  private[literal] sealed trait literal extends NonParenValueExpr

  implicit def stringToLiteral(s: String) = CharacterLiteral(s)
  implicit def numericToLiteral(n: Int) = NumericLiteral(n)
  implicit def booleanToLiteral(b: Boolean) = BooleanLiteral(b)

  case class CharacterLiteral(chars: String) extends literal {
    val toSql = "'" + chars.replace("'", "''") + "'"
  }
  case class NumericLiteral(num: Int) extends literal {
    val toSql = num.toString
    
  }
  case class BooleanLiteral(bool: Boolean) extends literal with BoolValueExpr{
    val toSql = bool.toString.toUpperCase
  }
}
