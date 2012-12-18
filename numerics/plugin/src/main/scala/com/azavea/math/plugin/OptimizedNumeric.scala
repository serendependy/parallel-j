package com.azavea.math.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.transform.InfoTransform
import nsc.transform.TypingTransformers
import nsc.symtab.Flags._
import nsc.ast.TreeDSL
import nsc.typechecker

/**
 * Our shiny compiler plugin.
 */
class OptimizedNumeric(val global: Global) extends Plugin {
  val name = "optimized-numeric"
  val description = "Optimizes com.azavea.math.Numeric usage."
  val components = List[PluginComponent](new RewriteInfixOps(this, global))
}

/**
 * This component turns things like:
 *   1. new FastNumericOps[T](m)(implicit ev).+(n)
 *   2. com.azavea.math.FastImplicits.infixOps[T](m)(implicit ev).*(n)
 *
 * Into:
 *   1. ev.plus(m, n)
 *   2. ev.times(m, n)
 */
class RewriteInfixOps(plugin:Plugin, val global:Global) extends PluginComponent
with Transform with TypingTransformers with TreeDSL {
  import global._
  import typer.typed

  // set to true to print a warning for each transform
  val debugging = false

  // TODO: maybe look up the definition of op and automatically figure mapping
  val unops = Map(
    newTermName("abs") -> "abs",
    newTermName("unary_$minus") -> "negate",
    newTermName("signum") -> "signum"
  )

  val binops = Map(
    newTermName("compare") -> "compare",
    newTermName("equiv") -> "equiv",
    newTermName("max") -> "max",
    newTermName("min") -> "min",

    newTermName("$less$eq$greater") -> "compare",
    newTermName("$div") -> "div",
    newTermName("$eq$eq$eq") -> "equiv",
    newTermName("$bang$eq$eq") -> "nequiv",
    newTermName("$greater") -> "gt",
    newTermName("$greater$eq") -> "gteq",
    newTermName("$less") -> "lt",
    newTermName("$less$eq") -> "lteq",
    newTermName("$minus") -> "minus",
    newTermName("$percent") -> "mod",
    newTermName("$plus") -> "plus",
    newTermName("$times") -> "times",
    newTermName("$times$times") -> "pow"
  )

  val runsAfter = List("typer");
  val phaseName = "optimized-numeric"
  def newTransformer(unit:CompilationUnit) = new MyTransformer(unit)

  // Determine if two type are equivalent
  def equivalentTypes(t1:Type, t2:Type) = {
    t1.dealias.deconst.widen =:= t2.dealias.deconst.widen
  }

  // TODO: figure out better type matching for Numeric, e.g. a.tpe <:< b.tpe
  val numericClass = definitions.getClass("com.azavea.math.Numeric")
  def isNumeric(t:Type) = t.typeSymbol == numericClass.tpe.typeSymbol

  // For built-in types, figure out whether or not we have a "fast" conversion method
  val BigIntClass = definitions.getClass("scala.math.BigInt")
  val BigDecimalClass = definitions.getClass("scala.math.BigDecimal")
  def getConverter(t:Type) = if (t <:< definitions.ByteClass.tpe) {
    Some("fromByte")
  } else if (t <:< definitions.ShortClass.tpe) {
    Some("fromShort")
  } else if (t <:< definitions.IntClass.tpe) {
    Some("fromInt")
  } else if (t <:< definitions.LongClass.tpe) {
    Some("fromLong")
  } else if (t <:< definitions.FloatClass.tpe) {
    Some("fromFloat")
  } else if (t <:< definitions.DoubleClass.tpe) {
    Some("fromDouble")
  } else if (t <:< BigIntClass.tpe) {
    Some("fromBigInt")
  } else if (t <:< BigDecimalClass.tpe) {
    Some("fromBigDecimal")
  } else {
    None
  }

  // TODO: maybe match further out on the implicit Numeric[T]?
  class MyTransformer(unit:CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      //def mylog(s:String) = if (debugging) unit.warning(tree.pos, s)
      def mylog(s:String) = Unit

      val tree2 = tree match {

        // match fuzzy binary operators
        case Apply(Apply(TypeApply(Select(Apply(Apply(_, List(m)), List(ev)), op), List(tt)), List(n)), List(ev2)) => {
          if (!isNumeric(ev.tpe)) {
            //mylog("fuzzy alarm #1")
            tree

          } else if (binops.contains(op)) {
            val op2 = binops(op)
            val conv = getConverter(n.tpe)
            conv match {
              case Some(meth) => {
                //mylog("fuzzy transformed %s (with %s)".format(op, meth))
                typed { Apply(Select(ev, op2), List(m, Apply(Select(ev, meth), List(n)))) }
              }
              case None => if (equivalentTypes(m.tpe, n.tpe)) {
                //mylog("fuzzy transformed %s (removed conversion)".format(op))
                typed { Apply(Select(ev, op2), List(m, n)) }
              } else {
                //mylog("fuzzy transformed %s".format(op))
                typed { Apply(Select(ev, op2), List(m, Apply(TypeApply(Select(ev, "fromType"), List(tt)), List(n)))) }
              }
            }

          } else {
            //mylog("fuzzy alarm #2")
            tree
          }
        }

        // match IntOps (and friends Float, Long, etc.)
        case Apply(Apply(TypeApply(Select(Apply(_, List(m)), op), List(tt)), List(n)), List(ev)) => {
          if (!isNumeric(ev.tpe)) {
            //mylog("literal ops alarm #1")
            tree
        
          } else if (binops.contains(op)) {
            val op2 = binops(op)
            val conv = getConverter(m.tpe)
            conv match {
              case Some(meth) => {
                //mylog("zzz literal ops transformed %s (with %s)".format(op, meth))
                typed { Apply(Select(ev, op2), List(Apply(Select(ev, meth), List(m)), n)) }
              }
              case None => {
                //mylog("zzz literal ops transformed %s".format(op))
                typed { Apply(Select(ev, op2), List(Apply(TypeApply(Select(ev, "fromType"), List(tt)), List(m)), n)) }
              }
            }
        
          } else {
            //mylog("literal ops alarm #2")
            tree
          }
        }

        // match binary operators
        case Apply(Select(Apply(Apply(_, List(m)), List(ev)), op), List(n)) => {
          if (!isNumeric(ev.tpe)) {
            unit.warning(tree.pos, "binop false alarm #1")
            tree
          } else if (binops.contains(op)) {
            val op2 = binops(op)
            //mylog("binop rewrote %s %s %s to n.%s(%s, %s)".format(m, op, n, op2, m, n))
            typed { Apply(Select(ev, op2), List(m, n)) }
          } else {
            unit.warning(tree.pos, "binop false alarm #2")
            tree
          }
        }

        // match unary operators
        case Select(Apply(Apply(_, List(m)), List(ev)), op) => {
          if (!isNumeric(ev.tpe)) {
            unit.warning(tree.pos, "unop false alarm #1")
            tree
          } else if (unops.contains(op)) {
            val op2 = unops(op)
            //mylog("unop rewrote %s to n.%s".format(op, op2))
            typed { Apply(Select(ev, op2), List(m)) }
          } else {
            unit.warning(tree.pos, "unop false alarm #2")
            tree
          }
        }

        case _ => tree
      }

      super.transform(tree2)
    }
  }
}
