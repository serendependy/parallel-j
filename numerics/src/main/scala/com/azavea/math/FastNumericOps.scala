package com.azavea.math

/**
 * @author Erik Osheim
 */

import scala.{specialized => spec}

/**
 * NumericOps adds things like inline operators to A. It's intended to
 * be used as an implicit decorator like so:
 *
 *   def foo[A:Numeric](a:A, b:A) = a + b
 *      (this is translated into) = new NumericOps(a).+(b)
 */
final class FastNumericOps[@spec(Int,Long,Float,Double) A:Numeric](val lhs:A) {
  val n = implicitly[Numeric[A]]

  def abs = n.abs(lhs)
  def unary_- = n.negate(lhs)
  def signum = n.signum(lhs)

  def compare(rhs:A) = n.compare(lhs, rhs)
  def equiv(rhs:A) = n.equiv(lhs, rhs)
  def max(rhs:A) = n.max(lhs, rhs)
  def min(rhs:A) = n.min(lhs, rhs)

  def <=>(rhs:A) = n.compare(lhs, rhs)
  def ===(rhs:A) = n.equiv(lhs, rhs)
  def !==(rhs:A) = n.nequiv(lhs, rhs)
  def >(rhs:A) = n.gt(lhs, rhs)
  def >=(rhs:A) = n.gteq(lhs, rhs)
  def <(rhs:A) = n.lt(lhs, rhs)
  def <=(rhs:A) = n.lteq(lhs, rhs)
  def /(rhs:A) = n.div(lhs, rhs)
  def -(rhs:A) = n.minus(lhs, rhs)
  def %(rhs:A) = n.mod(lhs, rhs)
  def +(rhs:A) = n.plus(lhs, rhs)
  def *(rhs:A) = n.times(lhs, rhs)
  def **(rhs:A) = n.pow(lhs, rhs)

  def toByte = n.toByte(lhs)
  def toShort = n.toShort(lhs)
  def toInt = n.toInt(lhs)
  def toLong = n.toLong(lhs)
  def toFloat = n.toFloat(lhs)
  def toDouble = n.toDouble(lhs)
  def toBigInt = n.toBigInt(lhs)
  def toBigDecimal = n.toBigDecimal(lhs)
}
