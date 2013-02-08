package j.lang.primitives

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.function.JVerb
import j.lang.datatypes.function.{JVerb1Type, JVerb2Type}
import j.lang.datatypes.array.{JArray, JArrayType}
import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.types._
import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.JArrayFlag._
import j.lang.datatypes.function.JVerb1Type

object JVerbs {
  
  val leftIdentity = new JVerb1Type[JArrayType](
      "[",
      List(JFuncRank(JInfinity)),
      (y: JArray[JArrayType]) => y,
      (x: JArray[JArrayType], y: JArray[JArrayType]) => x,
      jANY
  )
  val rightIdentity = new JVerb1Type[JArrayType](
      "]",
      List(JFuncRank(JInfinity)),
      (y: JArray[JArrayType]) => y,
      (x: JArray[JArrayType], y: JArray[JArrayType]) => y,
      jANY
  )
  val shapeReshape = new JVerb[JArrayType, JInt, JArrayType, JInt, JArrayType](
      "$",
      List[JFuncRank](JFuncRank(JInfinity, 1, JInfinity)),
      (y: JArray[JArrayType]) => {
        JArray(afNONE, jINT, 0, y.rank, List[Int](y.shape.length), y.shape.toArray)
      },
      (x: JArray[JInt], y: JArray[JArrayType]) => {
        val numItems:Int = x.ravel.fold(JReal.One)(_ * _).v
        val ravel:Array[JArrayType] = Array.tabulate(numItems)((i: Int) => 
          y.ravel(i % y.numItems) //TODO global fill used for fit
        ) //TODO modulo == slow performance
        JArray(afNONE, y.jaType, 0, numItems, x.ravel.toList.map(_ v), ravel)
      },
      jANY, jINT, jANY
  )
  
  val conjugatePlus = new JVerb1Type[JNumber](
      "+",
      List(JFuncRank(0)),
      (y: JArray[JNumber]) => {
        y(0) match {
          //TODO case c: JComplex => ...
          case a:JNumber => JArray.scalar(a)
        }
      },
      (x: JArray[JNumber], y: JArray[JNumber]) => {
        JArray(x.jaType | y.jaType, List(), Array(x.ravel(0) + y.ravel(0)))
      },
      jNUMERIC
  )
  
  val negateMinus = new JVerb1Type[JNumber](
      "-",
      List(JFuncRank(0)),
      (y: JArray[JNumber]) => {
        JArray.scalar(- y.ravel(0))
      },
      (x: JArray[JNumber], y: JArray[JNumber]) => {
        JArray.scalar(x.ravel(0) - y.ravel(0))
      },
      jNUMERIC
  )
  
  val boxLessthan = new JVerb[JArrayType, JReal, JReal, JBox, JInt](
      "<",
      List(JFuncRank(JInfinity, 0, 0)),
      (y: JArray[JArrayType]) => JArray.scalar(JBox(y)),
      (x: JArray[JReal], y: JArray[JReal]) => JArray.scalar(x.ravel(0) < y.ravel(0)),
      jANY, jNUMERIC, jNUMERIC
  )
  
  val integersIndex = new JVerb[JInt, JArrayType, JArrayType, JInt, JInt](
      "i.",
      List(JFuncRank(1, JInfinity, JInfinity)),
      (y: JArray[JInt]) => {
        JArray(jINT, y.ravel.toList, Array.tabulate(y.ravel.foldLeft(1)(_ * _))((x: Int) => x))
      },
      (x: JArray[JArrayType], y: JArray[JArrayType]) => {
        throw new Exception()//TODO implement
      },
      jINT, jANY, jANY
  )
  
//  val ravelAppend = new JVerb1Type[JArrayType](
//      ",",
//      List(JFuncRank(0)), //TODO should be infinity
//      (y: JArray[JArrayType]) => {
//        JArray(y.flag, y.jaType, 0, y.numItems, List(y.numItems), y.ravel)
//      },
//      (x: JArray[JArrayType], y: JArray[JArrayType]) => {
//        
//      },
//      jANY
//  )
//  val conjugatePlus = new JVerb1Type[JNumber](
//      "+",
//      List(JFuncRank(0)),
//      (y: JArray[JNumber]) => y(0) match {
//        case r: JReal => JArray[JNumber](r)
//      },
//      (x: JArray[JNumber], y: JArray[JNumber]) => x(0) + y(0),
//      jNUMERIC
//      )
      
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