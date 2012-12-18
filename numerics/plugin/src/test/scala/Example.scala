import com.azavea.math.Numeric

//import com.azavea.math.FastImplicits._
import com.azavea.math.EasyImplicits._
import Predef.{any2stringadd => _, _}

object Example {
  def foo1[T:Numeric](m:T, n:T) = m + n
  def foo2[T](m:T, n:T)(implicit ev:Numeric[T]) = ev.plus(m, n)

  def bar1[T:Numeric](m:T) = -m
  def bar2[T](m:T)(implicit ev:Numeric[T]) = ev.negate(m)

  def duh1[T:Numeric](m:T) = m + 13
  def duh2[T](m:T)(implicit ev:Numeric[T]) = ev.plus(m, ev.fromInt(13))

  def yak1[T:Numeric](m:T) = m + BigInt(9)
  def yak2[T](m:T)(implicit ev:Numeric[T]) = ev.plus(m, ev.fromBigInt(9))

  def zug1[T:Numeric](n:T) = 1 + n
  def zug2[T](n:T)(implicit ev:Numeric[T]) = ev.plus(ev.fromInt(1), n)

  def main(args: Array[String]) {
    println(bar1(1))
  }
}
