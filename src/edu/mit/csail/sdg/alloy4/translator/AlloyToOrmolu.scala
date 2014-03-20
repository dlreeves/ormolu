package edu.mit.csail.sdg.alloy4.translator

import edu.mit.csail.sdg.alloy4compiler.ast.{VisitReturn, Sig, ExprLet, ExprVar, ExprCall, ExprList, ExprQt, ExprConstant, ExprUnary, ExprBinary}
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field
import edu.mit.csail.sdg.alloy4.Env
import edu.mit.csail.sdg.ormolu.form.{Formula, True, False}
import scala.collection.JavaConversions._
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE
import edu.mit.csail.sdg.ormolu.rel._
import scala.collection.mutable.{HashSet, Map => MMap}
import edu.mit.csail.sdg.ormolu.{Predicate, Expression}

object AlloyToOrmolu extends VisitReturn[Option[Expression]] {
  private val env = new Env[ExprVar, Expression]
  private val atomSets: MMap[Sig, Option[AtomSet]] = MMap.empty[Sig, Option[AtomSet]]

  private def default = throw new RuntimeException

  private def default(x: String) = throw new RuntimeException(x)

  def visit(x: Sig): Option[AtomSet] =
    x match {
      case Sig.NONE => Some(Empty)
      case Sig.UNIV => Some(Univ)
      case state: Sig.PrimSig
        if state.label.contains("UnaryState") || state.label.contains("BinaryState") => Some(StateAtom(state.label))
      /*case state: Sig.SubsetSig
        if state.label.contains("UnaryState") || state.label.contains("BinaryState") => Some(StateAtom(state.parents.head.label))*/
      case p: Sig.PrimSig => for (parent <- atomSets getOrElseUpdate (x, visit(p.parent)))
      yield PrimAtom(p.label, parent)
      case s: Sig.SubsetSig => Some(SubSetAtom(s.label, s.parents flatMap {parent =>
        atomSets getOrElseUpdate (x, visit(parent))
      } toSeq: _*))
      case otherwise => default(otherwise.toString)
    }

  def visit(x: Field): Option[FieldRel] = for (owner <- visit(x.sig))
  yield visitThis(x.decl().expr) match {
      case Some(state:StateAtom) => StateField(x.label, owner, state)
      case Some(state: SubSetAtom)
        if state.parents.exists(_.isInstanceOf[StateAtom]) => StateField(x.label, owner, state.parents.filter(_.isInstanceOf[StateAtom]).head.asInstanceOf[StateAtom])
      case otherwise => NonStateField(x.label, owner, x.decl().expr.`type`.arity)
    }

  def visit(x: ExprLet): Option[Expression] = visitThis(x.expr) match {
    case Some(expr) => {
      env put (x.`var`, expr)
      for (ans <- visitThis(x.sub)) yield {
        env remove (x.`var`)
        ans
      }
    }
    case otherwise => default
  }

  def visit(x: ExprVar): Option[Expression] = {
    val ans = env get x
    if (ans == null) for (Relation(rel) <- visitThis(x.`type`.toExpr)) yield Variable(x.label, rel)
    else Some(ans)
  }

  def visit(x: ExprCall): Option[Expression] = {
    val exprVars = x.fun.params

    val rels = for {
      arg <- x.args
      Relation(rel) <- visitThis(arg)
    } yield rel

    if (rels.size != exprVars.size) default
    else {
      for {(exprVar, relation) <- exprVars zip rels} env.put(exprVar, relation)

      val ans = visitThis(x.fun.getBody)
      exprVars foreach {
        env.remove
      }

      ans
    }
  }

  def visit(x: ExprITE): Option[Expression] = for (c <- visitThis(x.cond); l <- visitThis(x.left); r <- visitThis(x.right))
  yield (c, l, r) match {
      case (Formula(cond), Formula(left), Formula(right)) => cond then left otherwise right
      //case (Formula(cond), Relation(left), Relation(right)) => cond then left otherwise right
      case otherwise => default(otherwise + " - " + x)
    }

  def visit(x: ExprList): Option[Expression] = {
    import ExprList.Op._

    x.op match {
      case AND => {
        val formulas = for (a <- x.args; Formula(f) <- visitThis(a)) yield f
        val conjunction = if (formulas.size > 1) Option(formulas.reduceLeft {
          _ and _
        })
        else formulas.headOption
        if (formulas.size == x.args.size) conjunction else default
      }
      case OR => {
        val formulas = for (a <- x.args; Formula(f) <- visitThis(a)) yield f
        val disjunction = if (formulas.size > 1) Option(formulas.reduceLeft {
          _ or _
        })
        else formulas.headOption
        if (formulas.size == x.args.size) disjunction else default
      }
      /*case TOTALORDER => for {
        Relation(elem) <- visitThis(x.args get 0)
        Relation(first) <- visitThis(x.args get 1)
        Relation(next) <- visitThis(x.args get 2)
      } yield TotalOrder(elem, first, next)*/
      case otherwise => default(otherwise.toString + " - " + x)
    }
  }

  def visit(x: ExprQt): Option[Expression] = {
    import Formula._
    import ExprQt.Op._
    x.desugar match {
      case xx: ExprQt => {
        val args = for (i <- 0 until xx.count; Relation(rel) <- visitThis(xx getBound i)) yield Variable(xx.get(i).label, rel)

        if (args.size != xx.count) default
        else {
          //for(i <- 0 until xx.count) env.put(xx.get(i), args(i).relation)
          val rel = xx.op match {
            case ALL => for (Formula(sub) <- visitThis(xx.sub)) yield all(args: _*) {
              sub
            }
            case NO => for (Formula(sub) <- visitThis(xx.sub)) yield no(args: _*) {
              sub
            }
            case ONE => for (Formula(sub) <- visitThis(xx.sub)) yield one(args: _*) {
              sub
            }
            case LONE => for (Formula(sub) <- visitThis(xx.sub)) yield lone(args: _*) {
              sub
            }
            case COMPREHENSION => for (Formula(sub) <- visitThis(xx.sub)) yield {
              Comprehension(sub, args:_*)
            }
            case otherwise => default
          }
        // for(i <- 0 until xx.count) env.remove(xx.get(i))
          rel
        }
      }
      case xx => visitThis(xx)
    }
  }

  def visit(x: ExprConstant): Option[Expression] = {
    import ExprConstant.Op._
    x.op match {
      case TRUE => Some(True)
      case FALSE => Some(False)
      case EMPTYNESS => Some(Empty)
      case IDEN => Some(Iden)
      case otherwise => default
    }
  }

  def visit(x: ExprUnary): Option[Expression] = {
    import Relation._
    import ExprUnary.Op._

    x.op match {
      case NOT => for (Formula(sub) <- visitThis(x.sub)) yield !sub
      case SOME => for (Relation(sub) <- visitThis(x.sub)) yield some(sub)
      case LONE => for (Relation(sub) <- visitThis(x.sub)) yield lone(sub)
      case ONE => for (Relation(sub) <- visitThis(x.sub)) yield one(sub)
      case NO => for (Relation(sub) <- visitThis(x.sub)) yield no(sub)
      case TRANSPOSE => for (Relation(sub) <- visitThis(x.sub)) yield ~sub
//      case RCLOSURE => for (Relation(sub) <- visitThis(x.sub)) yield *(sub)
//      case CLOSURE => for (Relation(sub) <- visitThis(x.sub)) yield ^(sub)
      case mult@(EXACTLYOF | SOMEOF | LONEOF | ONEOF | SETOF) =>
        for (Relation(sub) <- visitThis(x.sub)) yield sub
      case NOOP => visitThis(x.sub)
      case otherwise => default(x.op.toString)
    }
  }

  def visit(x: ExprBinary): Option[Expression] = {
    import ExprBinary.Op._

    x.op match {
      case EQUALS => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left === right
      case NOT_EQUALS => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield !(left === right)
      case IMPLIES => for (Formula(left) <- visitThis(x.left); Formula(right) <- visitThis(x.right)) yield left ==> right
      case IFF => for (Formula(left) <- visitThis(x.left); Formula(right) <- visitThis(x.right)) yield left <=> right
      case AND => for (Formula(left) <- visitThis(x.left); Formula(right) <- visitThis(x.right)) yield left and right
      case OR => for (Formula(left) <- visitThis(x.left); Formula(right) <- visitThis(x.right)) yield left or right
      case IN => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left in right
      case NOT_IN => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield !(left in right)
      case PLUSPLUS => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left ++ right
      case PLUS => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left + right
      case MINUS => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left - right
      case INTERSECT => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left & right
      case JOIN => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left >< right
      case DOMAIN => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left <:< right
      case RANGE => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left >:> right
      case op@(ARROW | ANY_ARROW_SOME | ANY_ARROW_ONE | ANY_ARROW_LONE | SOME_ARROW_ANY | SOME_ARROW_SOME | SOME_ARROW_ONE |
               SOME_ARROW_LONE | ONE_ARROW_ANY | ONE_ARROW_SOME | ONE_ARROW_ONE | ONE_ARROW_LONE | LONE_ARROW_ANY | LONE_ARROW_SOME |
               LONE_ARROW_ONE | LONE_ARROW_LONE) => for (Relation(left) <- visitThis(x.left); Relation(right) <- visitThis(x.right)) yield left -> right
      case otherwise => default

    }

  }

}