package j.lang.primitives

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.function.JVerb

import j.lang.datatypes.array.{JArray, JArrayType}
import j.lang.datatypes.array.JArrayType._

import scala.Numeric.Implicits._

object JVerbs {
//	val conjugatePlus: JVerb[Numeric[A], Numeric[B], Numeric[C], Numeric[D], Numeric[E]] = Unit
  
   def plus[T : JNUMERIC] (x: JArray[T], y:JArray[T]) = {
	   x.ravel.zip(y.ravel).map((x: (T,T)) => () )
   }
   
   def square[A : Numeric](a:A) = implicitly[Numeric[A]].times(a,a)
  
   def sum[T : JNUMERIC](x: T, y: T) = x + y
   
   def sum[U : JNUMERIC, T <% U , S <% U](x: T, y: S) = implicitly[Numeric[U]].plus(x,y)
}