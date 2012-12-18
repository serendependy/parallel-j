package com.azavea.math

import scala.{specialized => spec}

/**
 * IntOps, LongOps and friends provide the same tilde operators as NumericOps
 * (such as +~, -~, *~, etc) for the number types we're interested in.
 *
 * Using these, we use these operators with literals, number types, and
 * generic types. For instance:
 *
 *   def foo[A:Numeric](a:A, b:Int) = (a *~ b) +~ 1
 */

final class LiteralIntOps(val lhs:Int) {
  def compare[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromInt(lhs), rhs)
  def equiv[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromInt(lhs), rhs)
  def max[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.max(n.fromInt(lhs), rhs)
  def min[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.min(n.fromInt(lhs), rhs)

  def <=>[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromInt(lhs), rhs)
  def ===[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromInt(lhs), rhs)
  def !==[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.nequiv(n.fromInt(lhs), rhs)

  def /[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.div(n.fromInt(lhs), rhs)
  def >[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gt(n.fromInt(lhs), rhs)
  def >=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gteq(n.fromInt(lhs), rhs)
  def <[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lt(n.fromInt(lhs), rhs)
  def <=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lteq(n.fromInt(lhs), rhs)
  def -[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.minus(n.fromInt(lhs), rhs)
  def %[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.mod(n.fromInt(lhs), rhs)
  def +[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.plus(n.fromInt(lhs), rhs)
  def *[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.times(n.fromInt(lhs), rhs)
  def **[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.pow(n.fromInt(lhs), rhs)
  def toNumeric[@spec(Int,Long,Float,Double) A](implicit n:Numeric[A]) = n.fromInt(lhs)

  def toBigInt() = BigInt(lhs)
  def toBigDecimal() = BigDecimal(lhs)
}


final class LiteralLongOps(val lhs:Long) {
  def compare[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromLong(lhs), rhs)
  def equiv[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromLong(lhs), rhs)
  def max[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.max(n.fromLong(lhs), rhs)
  def min[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.min(n.fromLong(lhs), rhs)

  def <=>[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromLong(lhs), rhs)
  def ===[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromLong(lhs), rhs)
  def !==[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.nequiv(n.fromLong(lhs), rhs)

  def /[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.div(n.fromLong(lhs), rhs)
  def >[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gt(n.fromLong(lhs), rhs)
  def >=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gteq(n.fromLong(lhs), rhs)
  def <[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lt(n.fromLong(lhs), rhs)
  def <=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lteq(n.fromLong(lhs), rhs)
  def -[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.minus(n.fromLong(lhs), rhs)
  def %[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.mod(n.fromLong(lhs), rhs)
  def +[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.plus(n.fromLong(lhs), rhs)
  def *[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.times(n.fromLong(lhs), rhs)
  def **[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.pow(n.fromLong(lhs), rhs)
  def toNumeric[@spec(Int,Long,Float,Double) A](implicit n:Numeric[A]) = n.fromLong(lhs)

  def toBigInt() = BigInt(lhs)
  def toBigDecimal() = BigDecimal(lhs)
}


final class LiteralFloatOps(val lhs:Float) {
  def compare[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromFloat(lhs), rhs)
  def equiv[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromFloat(lhs), rhs)
  def max[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.max(n.fromFloat(lhs), rhs)
  def min[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.min(n.fromFloat(lhs), rhs)

  def <=>[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromFloat(lhs), rhs)
  def ===[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromFloat(lhs), rhs)
  def !==[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.nequiv(n.fromFloat(lhs), rhs)

  def /[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.div(n.fromFloat(lhs), rhs)
  def >[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gt(n.fromFloat(lhs), rhs)
  def >=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gteq(n.fromFloat(lhs), rhs)
  def <[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lt(n.fromFloat(lhs), rhs)
  def <=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lteq(n.fromFloat(lhs), rhs)
  def -[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.minus(n.fromFloat(lhs), rhs)
  def %[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.mod(n.fromFloat(lhs), rhs)
  def +[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.plus(n.fromFloat(lhs), rhs)
  def *[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.times(n.fromFloat(lhs), rhs)
  def **[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.pow(n.fromFloat(lhs), rhs)
  def toNumeric[@spec(Int,Long,Float,Double) A](implicit n:Numeric[A]) = n.fromFloat(lhs)

  def toBigInt() = BigDecimal(lhs).toBigInt
  def toBigDecimal() = BigDecimal(lhs)
}


final class LiteralDoubleOps(val lhs:Double) {
  def compare[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromDouble(lhs), rhs)
  def equiv[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromDouble(lhs), rhs)
  def max[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.max(n.fromDouble(lhs), rhs)
  def min[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.min(n.fromDouble(lhs), rhs)

  def <=>[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromDouble(lhs), rhs)
  def ===[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromDouble(lhs), rhs)
  def !==[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.nequiv(n.fromDouble(lhs), rhs)

  def /[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.div(n.fromDouble(lhs), rhs)
  def >[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gt(n.fromDouble(lhs), rhs)
  def >=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gteq(n.fromDouble(lhs), rhs)
  def <[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lt(n.fromDouble(lhs), rhs)
  def <=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lteq(n.fromDouble(lhs), rhs)
  def -[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.minus(n.fromDouble(lhs), rhs)
  def %[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.mod(n.fromDouble(lhs), rhs)
  def +[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.plus(n.fromDouble(lhs), rhs)
  def *[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.times(n.fromDouble(lhs), rhs)
  def **[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.pow(n.fromDouble(lhs), rhs)
  def toNumeric[@spec(Int,Long,Float,Double) A](implicit n:Numeric[A]) = n.fromDouble(lhs)

  def toBigInt() = BigDecimal(lhs).toBigInt
  def toBigDecimal() = BigDecimal(lhs)
}


final class LiteralBigIntOps(val lhs:BigInt) {
  def compare[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromBigInt(lhs), rhs)
  def equiv[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromBigInt(lhs), rhs)
  def max[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.max(n.fromBigInt(lhs), rhs)
  def min[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.min(n.fromBigInt(lhs), rhs)

  def <=>[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromBigInt(lhs), rhs)
  def ===[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromBigInt(lhs), rhs)
  def !==[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.nequiv(n.fromBigInt(lhs), rhs)

  def /[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.div(n.fromBigInt(lhs), rhs)
  def >[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gt(n.fromBigInt(lhs), rhs)
  def >=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gteq(n.fromBigInt(lhs), rhs)
  def <[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lt(n.fromBigInt(lhs), rhs)
  def <=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lteq(n.fromBigInt(lhs), rhs)
  def -[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.minus(n.fromBigInt(lhs), rhs)
  def %[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.mod(n.fromBigInt(lhs), rhs)
  def +[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.plus(n.fromBigInt(lhs), rhs)
  def *[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.times(n.fromBigInt(lhs), rhs)
  def **[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.pow(n.fromBigInt(lhs), rhs)
  def toNumeric[@spec(Int,Long,Float,Double) A](implicit n:Numeric[A]) = n.fromBigInt(lhs)

  def toBigInt() = lhs
  def toBigDecimal() = BigDecimal(lhs)
}


final class LiteralBigDecimalOps(val lhs:BigDecimal) {
  def compare[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromBigDecimal(lhs), rhs)
  def equiv[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromBigDecimal(lhs), rhs)
  def max[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.max(n.fromBigDecimal(lhs), rhs)
  def min[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.min(n.fromBigDecimal(lhs), rhs)

  def <=>[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.compare(n.fromBigDecimal(lhs), rhs)
  def ===[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.equiv(n.fromBigDecimal(lhs), rhs)
  def !==[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.nequiv(n.fromBigDecimal(lhs), rhs)

  def /[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.div(n.fromBigDecimal(lhs), rhs)
  def >[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gt(n.fromBigDecimal(lhs), rhs)
  def >=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.gteq(n.fromBigDecimal(lhs), rhs)
  def <[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lt(n.fromBigDecimal(lhs), rhs)
  def <=[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.lteq(n.fromBigDecimal(lhs), rhs)
  def -[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.minus(n.fromBigDecimal(lhs), rhs)
  def %[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.mod(n.fromBigDecimal(lhs), rhs)
  def +[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.plus(n.fromBigDecimal(lhs), rhs)
  def *[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.times(n.fromBigDecimal(lhs), rhs)
  def **[@spec(Int,Long,Float,Double) A](rhs:A)(implicit n:Numeric[A]) = n.pow(n.fromBigDecimal(lhs), rhs)
  def toNumeric[@spec(Int,Long,Float,Double) A](implicit n:Numeric[A]) = n.fromBigDecimal(lhs)

  def toBigInt() = lhs.toBigInt
  def toBigDecimal() = lhs
}
