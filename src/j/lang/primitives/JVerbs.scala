package j.lang.primitives

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.function.JVerb
import j.lang.datatypes.function.JVerb1Type

import j.lang.datatypes.array.{JArray, JArrayType}

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.types._
import j.lang.datatypes.array.types.TypeImplicits._


object JVerbs {
  
  val leftIdentity = new JVerb1Type[JArrayType](
      "[",
      List(JFuncRank(JInfinity,JInfinity,JInfinity)),
      (y: JArray[JArrayType]) => y,
      (x: JArray[JArrayType], y: JArray[JArrayType]) => x,
      jANY
  )
  
//  val conjugatePlus = new JVerb[JNumber,JNumber,JNumber,JNumber,JNumber](
//	  "+",
//	  List(JFuncRank(0,0,0)),
//	  _,
//		  
//  )
//	val conjugatePlus: JVerb[Numeric[A], Numeric[B], Numeric[C], Numeric[D], Numeric[E]] = Unit
  
//   def plus[T : JNUMERIC] (x: JArray[T], y:JArray[T]) = {
//	   x.ravel.zip(y.ravel).map((x: (T,T)) => () )
//   }
//   
//   def square[A : Numeric](a:A) = implicitly[Numeric[A]].times(a,a)
//
//   def sum[U : JNUMERIC, T <% U , S <% U](x: T, y: S) = implicitly[Numeric[U]].plus(x,y)
}