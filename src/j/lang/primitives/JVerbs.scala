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
  
  final object leftIdentity extends JVerb1Type[JArrayType](
      "[",
      List(JFuncRank(JInfinity)),
      jANY
  ) {

    override def monadImpl[T <: JArray[JArrayType]](y: T): T = y
    override def dyadImpl[T1 <: JArray[JArrayType], T2 <: JArray[JArrayType]](x: T1, y: T2): T1 = x
  }
//  val leftIdentity = new JVerb1Type[JArrayType](
//      "[",
//      List(JFuncRank(JInfinity)),
//      (y: JArray[JArrayType]) => y,
//      (x: JArray[JArrayType], y: JArray[JArrayType]) => x,
//      jANY
//  )

  final object rightIdentity extends JVerb1Type[JArrayType](
      "]",
      List(JFuncRank(JInfinity)),
      jANY
  )
  {
    override def monadImpl(y: JArray[JArrayType]) = y
    override def dyadImpl(x: JArray[JArrayType], y: JArray[JArrayType]) = y
  }
//  val rightIdentity = new JVerb1Type[JArrayType](
//      "]",
//      List(JFuncRank(JInfinity)),
//      (y: JArray[JArrayType]) => y,
//      (x: JArray[JArrayType], y: JArray[JArrayType]) => y,
//      jANY
//  )
  
  final object shapeReshape extends JVerb[JArrayType, JInt, JArrayType, JInt, JArrayType](
      "$",
      List(JFuncRank(JInfinity, 1, JInfinity)),
      jANY, jINT, jANY
  ) {
    override def monadImpl(y: JArray[JArrayType]) = 
      JArray(afNONE, jINT, 0, y.rank, List[Int](y.shape.length), Vector() ++ y.shape)
      
    override def dyadImpl(x: JArray[JInt], y: JArray[JArrayType]) = {
      val numItems: Int = x.ravel.fold(JReal.One)(_ * _)
      val ravel = Vector.tabulate(numItems)((i: Int) =>
        y.ravel(i % y.numItems)) //TODO global fill used for !. (fit)
      JArray(afNONE, y.jaType, 0, numItems, x.ravel.toList.map(_ v), ravel)
    } //TODO modulo is slow
  }
//  val shapeReshape = new JVerb[JArrayType, JInt, JArrayType, JInt, JArrayType](
//      "$",
//      List[JFuncRank](JFuncRank(JInfinity, 1, JInfinity)),
//      (y: JArray[JArrayType]) => {
//        JArray(afNONE, jINT, 0, y.rank, List[Int](y.shape.length), Vector() ++ y.shape)
//      },
//      (x: JArray[JInt], y: JArray[JArrayType]) => {
//        val numItems:Int = x.ravel.fold(JReal.One)(_ * _).v
//        val ravel = Vector.tabulate(numItems)((i: Int) => 
//          y.ravel(i % y.numItems) //TODO global fill used for fit
//        ) //TODO modulo == slow performance
//        JArray(afNONE, y.jaType, 0, numItems, x.ravel.toList.map(_ v), ravel)
//      },
//      jANY, jINT, jANY
//  )
//  
  final object conjugatePlus extends JVerb1Type[JNumber](
      "+",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    
    override def monadImpl(y: JArray[JNumber]) = y(0) match {
      //TODO case c: JComplex => ...
      case a:JNumber => y(0)
    }
    
    override def dyadImpl(x: JArray[JNumber], y: JArray[JNumber]) = 
      JArray(x.jaType | y.jaType, List(), Vector(x.ravel(0) + y.ravel(0)))
  }
//  val conjugatePlus = new JVerb1Type[JNumber](
//      "+",
//      List(JFuncRank(0)),
//      (y: JArray[JNumber]) => {
//        y(0) match {
//          //TODO case c: JComplex => ...
//          case a:JNumber => JArray.scalar(a)
//        }
//      },
//      (x: JArray[JNumber], y: JArray[JNumber]) => {
//        JArray(x.jaType | y.jaType, List(), Vector(x.ravel(0) + y.ravel(0)))
//      },
//      jNUMERIC
//  )
//  
  final object negateMinus extends JVerb1Type[JNumber](
      "-",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monadImpl(y: JArray[JNumber]) = JArray.scalar(- y.ravel(0))
    override def dyadImpl(x: JArray[JNumber], y: JArray[JNumber]) =
      JArray.scalar(x.ravel(0) - y.ravel(0))
  }
//  val negateMinus = new JVerb1Type[JNumber](
//      "-",
//      List(JFuncRank(0)),
//      (y: JArray[JNumber]) => {
//        JArray.scalar(- y.ravel(0))
//      },
//      (x: JArray[JNumber], y: JArray[JNumber]) => {
//        JArray.scalar(x.ravel(0) - y.ravel(0))
//      },
//      jNUMERIC
//  )
//  
//  val boxLessthan = new JVerb[JArrayType, JReal, JReal, JBox, JInt](
//      "<",
//      List(JFuncRank(JInfinity, 0, 0)),
//      (y: JArray[JArrayType]) => JArray.scalar(JBox(y)),
//      (x: JArray[JReal], y: JArray[JReal]) => JArray.scalar(x.ravel(0) < y.ravel(0)),
//      jANY, jNUMERIC, jNUMERIC
//  )
//  
  final object integersIndex extends JVerb[JInt, JArrayType, JArrayType, JInt, JInt](
      "i.",
      List(JFuncRank(1, JInfinity, JInfinity)),
      jINT, jANY, jANY
   ){

    override def monadImpl(y: JArray[JInt]) = 
      JArray(jINT, y.ravel.toList, Vector.tabulate(y.ravel.foldLeft(1)(_ * _))((x: Int) => x))
    
    override def dyadImpl[T1 <: JArray[JArrayType]](x: T1, y: T1) = {
      throw new Exception() //TODO implement
    }
  }
//  val integersIndex = new JVerb[JInt, JArrayType, JArrayType, JInt, JInt](
//      "i.",
//      List(JFuncRank(1, JInfinity, JInfinity)),
//      (y: JArray[JInt]) => {
//        JArray(jINT, y.ravel.toList, Vector.tabulate(y.ravel.foldLeft(1)(_ * _))((x: Int) => x))
//      },
//      (x: JArray[JArrayType], y: JArray[JArrayType]) => {
//        throw new Exception()//TODO implement
//      },
//      jINT, jANY, jANY
//  )
//  
//  val ravelAppend = new JVerb1Type[JArrayType](
//      ",",
//      List(JFuncRank(JInfinity)),
//      (y: JArray[JArrayType]) => {
//        JArray(y.flag, y.jaType, 0, y.numItems, List(y.numItems), y.ravel)
//      },
//      (x: JArray[JArrayType])
//  )
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
}