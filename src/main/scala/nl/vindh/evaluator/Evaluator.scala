package nl.vindh.evaluator

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.internal.Flags._

object Evaluator {
  def evaluate(expr: Any): Any = throw new Exception("This call should be eliminated by a macro.")

  def defining(expr: Any): Any /*String*/ = macro definingImpl

  def definingImpl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {//c.Expr[String] = {
    import c.universe._
    val block = expr.tree


    /*Block(
      List(
        ValDef(Modifiers(), TermName("y"), TypeTree(), Literal(Constant(4))),
        DefDef(
          Modifiers(),
          TermName("f"),
          List(),
          List(
            List(
              ValDef(Modifiers(PARAM), TermName("n"), TypeTree().setOriginal(Select(Ident(scala), scala.Int)), EmptyTree),
              ValDef(Modifiers(PARAM), TermName("m"), TypeTree().setOriginal(Select(Ident(scala), scala.Int)), EmptyTree)
            )
          ),
          TypeTree().setOriginal(Select(Ident(scala), scala.Int)),
          Apply(
            Select(
              Ident(TermName("n")),
              TermName("$plus")
            ),
            List(Ident(TermName("m")))
          )
        )
      ),
      Apply(
        Select(Ident(nl.vindh.evaluator.Evaluator), TermName("evaluate")),
        List(
          Apply(
            Select(
              Apply(
                Select(
                  Literal(Constant(2)),
                  TermName("$plus")
                ),
                List(
                  Ident(TermName("y"))
                )
              ),
              TermName("$times")
            ),
            List(
              Apply(
                Ident(TermName("f")),
                List(
                  Literal(Constant(1)),
                  Literal(Constant(2))
                )
              )
            )
          )
        )
      )
    )*/

    // TODO: implement an extractor for evaluate(tree)
    def getToBeEvaluated: Option[Tree] = block match {
      case Block(_, Apply(Select(_, TermName("evaluate")), List(t: Tree))) => Some(t)
      case _ => None
    }


    def getDefByName(name: String): Option[Tree] = block match {
      case Block(lst: List[c.universe.Tree], _) => lst.find{
        _ match {
          case DefDef(_, TermName(name), _, _, _, _) => true
          case _ => false
        }
      }
      case _ => None
    }


    def getDefBody(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, b) => b
    }

    // Note that this looks only in the first argument list
    def getNthParamName(tree: Tree, n: Int): String = tree match {
      case DefDef(_, _, _, List(lst: List[Tree]), _, _) => lst(n) match {
        case ValDef(_, TermName(name), _, _) => name
      }
      case _ => {
        println(s"No match found in getNthParamName: $tree")
        throw new Exception
      }
    }

    def substituteValue(tree: Tree, varName: String, value: Tree): Tree = tree match {
      case Apply(f, arglist) => Apply(substituteValue(f, varName, value), arglist.map(substituteValue(_, varName, value)))
      case Ident(TermName(n)) if n == varName => value
      case Select(qualifier, name) => Select(substituteValue(qualifier, varName, value), name)
      case t: Tree => t.
      case t => println(showRaw(t)); t
    }

    def substituteDeep(tree: Tree): Option[Tree] = tree match {
      case Apply(f, arglist) => substituteDeepList(arglist) match {
        case Some(lst) => Some(Apply(f, lst))
        case None => f match {
          case Ident(TermName(name: String)) => {
            substituteFunction(name, arglist)
          }
          case TypeApply(Ident(TermName(name: String)), _) => substituteFunction(name, arglist)
          case t => {
            println(showRaw(t))
            None
          }
        }
      }
      case _ => None
    }

    def substituteFunction(name: String, arglist: List[Tree]): Option[Tree] = {
      val defDef = getDefByName(name).get // Not nice!
      val defBody = getDefBody(defDef)

      Some(arglist.zipWithIndex.foldRight(defBody){
        (argWithIndex, body) =>
          val argName = getNthParamName(defDef, argWithIndex._2)
          substituteValue(body, argName, argWithIndex._1)
      })
    }

    def substituteDeepList(lst: List[Tree]): Option[List[Tree]] = lst match {
      case Nil => None
      case hd :: tl => substituteDeep(hd) match {
        case Some(t) => Some(t :: tl)
        case None => substituteDeepList(tl) match {
          case Some(lst) => Some(hd :: lst)
          case None => None
        }
      }
    }

    val s = getToBeEvaluated match {
      case Some(t) => substituteDeep(t)
      case None => {
        println("Nothing to evaluate")
        None
      }
    }

    //val s2 = substituteDeep(s.get)

    //val s3 = substituteDeep(s2.get)

    println(s"s: $s")
    //println(s"s2: $s2")
    //println(s"s3: $s3")


println(s"\n\nraw: ${showRaw(expr)}")
    expr
  }
}


