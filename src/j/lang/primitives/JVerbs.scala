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

    override def monad[T <: JArray[JArrayType]](y: T) = y
    override def dyad[T1 <: JArray[JArrayType], T2 <: JArray[JArrayType]](x: T1, y: T2) = x
  }

  final object rightIdentity extends JVerb1Type[JArrayType](
      "]",
      List(JFuncRank(JInfinity)),
      jANY
  )
  {
    override def monad[T <: JArray[JArrayType]](y: T) = y
    override def dyad[T1 <: JArray[JArrayType], T2 <: JArray[JArrayType]](x: T1, y: T2) = y
  }
  
  final object shapeReshape extends JVerb[JArrayType, JInt, JArrayType, JInt, JArrayType](
      "$",
      List(JFuncRank(JInfinity, 1, JInfinity)),
      jANY, jINT, jANY
  ) {
    override def monad[T <: JArray[JArrayType]](y: T) = 
      JArray(afNONE, jINT, 0, y.rank, List[Int](y.shape.length), Vector() ++ y.shape)
      
    override def dyad[T1 <: JArray[JInt], T2 <: JArray[JArrayType]](x: T1, y: T2) = {
      val numItems: Int = x.ravel.fold(JReal.One)(_ * _)
      val ravel = Vector.tabulate(numItems)((i: Int) =>
        y.ravel(i % y.numScalars)) //TODO global fill used for !. (fit)
      JArray(afNONE, y.jaType, 0, numItems, x.ravel.toList.map(_ v), ravel)
    } //TODO modulo is slow
  }

  final object conjugatePlus extends JVerb1Type[JNumber](
      "+",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    
    override def monad[T <: JArray[JNumber]](y: T) = y(0) match {
      //TODO case c: JComplex => ...
      case a:JNumber => y(0)
    }
    
    override def dyad[T1 <: JArray[JNumber], T2 <: JArray[JNumber]](x: T1, y: T2) = 
      JArray(x.jaType | y.jaType, List(), Vector(x.ravel(0) + y.ravel(0)))
  }
  
  final object negateMinus extends JVerb1Type[JNumber](
      "-",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monad[T <: JArray[JNumber]](y: T) = JArray.scalar(- y.ravel(0))
    override def dyad[T1 <: JArray[JNumber], T2 <: JArray[JNumber]](x: T1, y: T2) =
      JArray.scalar(x.ravel(0) - y.ravel(0))
  }
 
  final object integersIndex extends JVerb[JInt, JArrayType, JArrayType, JInt, JInt](
      "i.",
      List(JFuncRank(1, JInfinity, JInfinity)),
      jINT, jANY, jANY
   ){

    override def monad[T <: JArray[JInt]](y: T): JArray[JInt] = 
      JArray(jINT, y.ravel.toList, Vector.tabulate(y.ravel.foldLeft(1)(_ * _))((x: Int) => x))
    
    override def dyad[T1 <: JArray[_], T2 <: JArray[_]](x: T1, y: T2) = {
      throw new Exception() //TODO implement
    }
  }

  
  final object ravelAppend extends JVerb1Type[JArrayType](
      ",",
      List(JFuncRank(JInfinity)),
      jANY
  ){
    override def monad[T <: JArray[JArrayType]](y: T) = {
      JArray(y.jaType, List(y.numScalars), y.ravel)
    }
    
    //TODO kill myself. Why have fancy type checking if I still have to do this?
    override def dyad[T1 <: JArray[JArrayType], T2 <: JArray[JArrayType]](x: T1, y: T2): T1 = y match {
        case isT1:T1 => {
          throw new Exception() //trickier than I thought
        }
        case _ => throw new Exception()
    }
  }
  
  final object incrementGreaterthan extends JVerb[JNumber, JReal, JReal, JNumber, JInt](
      ">:",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monad[T <: JArray[JNumber]](y: T) = JArray(y.jaType, List(), Vector(y.ravel(0) + 1))
    override def dyad[T1 <: JArray[JReal], T2 <: JArray[JReal]](x: T1, y: T2) = {
      JArray(jINT, List(), Vector(if (x.ravel(0) >= y.ravel(0)) 1 else 0))
    }
  }
  
  final object decrementLesserthan extends JVerb[JNumber, JReal, JReal, JNumber, JInt](
      "<:",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monad[T <: JArray[JNumber]](y: T) = JArray(y.jaType, List(), Vector(y.ravel(0) - 1))
    override def dyad[T1 <: JArray[JReal], T2 <: JArray[JReal]](x: T1, y: T2) = {
      JArray(jINT, List(), Vector(if (x.ravel(0) <= y.ravel(0)) 1 else 0))
    }
  }
  
  final object floorLesserof extends JVerb[JNumber, JReal, JReal, JNumber, JReal](
      "<.",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monad[T <: JArray[JNumber]](y: T) = y.ravel(0) match {
      case inf: JInfinite => y
      case int: JInt => y
      case r: JFloat => JArray(jINT, List(), Vector(r.v.toInt))
      case _ => throw new Exception() //TODO implement
    }
    
    override def dyad[T1 <: JArray[JReal], T2 <: JArray[JReal]](x: T1, y: T2) = {
      JArray(jNUMERIC, List(), Vector(if (x.ravel(0) < y.ravel(0)) x.ravel(0) else y.ravel(0)))
    }
  }
  
  final object ceilingGreaterof extends JVerb[JNumber, JReal, JReal, JNumber, JReal](
      ">.",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monad[T <: JArray[JNumber]](y: T) = y.ravel(0) match {
      case inf: JInfinite => y
      case int: JInt => y
      case r: JFloat => JArray(jINT, List(), Vector(r.v.+(0.5).toInt))
      case _ => throw new Exception() //TODO implement
    }
    
    override def dyad[T1 <: JArray[JReal], T2 <: JArray[JReal]](x: T1, y: T2) = {
      JArray(jNUMERIC, List(), Vector(if (x.ravel(0) < y.ravel(0)) y.ravel(0) else x.ravel(0)))
    }
  }
  
  final object naturalExponent extends JVerb1Type[JNumber](
      "^",
      List(JFuncRank(0)),
      jNUMERIC
  ) {
    override def monad[T <: JArray[JNumber]](y: T) =
      JArray.scalar(JReal.E ** y.ravel(0))
    
    override def dyad[T1 <: JArray[JNumber], T2 <: JArray[JNumber]](x: T1, y: T2) =
      JArray.scalar(x.ravel(0) ** y.ravel(0))
  }

  final object naturalLog extends JVerb1Type[JNumber](
      "^.",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monad[T <: JArray[JNumber]](y: T) = {
      JArray.scalar(JReal.E %% y.ravel(0))
    }
    
    override def dyad[T1 <: JArray[JNumber], T2 <: JArray[JNumber]](x:T1, y: T2) = {
      JArray.scalar(x.ravel(0) %% y.ravel(0))
    }
  }
  
  final object reverseShift extends JVerb[JArrayType, JInt, JArrayType, JArrayType, JArrayType](
      "|.",
      List(JFuncRank(0, JInfinity, 0)),
      jANY, jINT, jANY
  ){
    override def monad[T <: JArray[JArrayType]](y: T) = {
      JArray(y.jaType, y.shape,
          (0 until y.numItemz).reverse.map(
              i => y.ravel.slice(i, i*y.itemSize)).foldLeft(
                  Vector[JArrayType]())(_ ++ _))
    }
    
    override def dyad[T1 <: JArray[JInt], T2 <: JArray[JArrayType]](x: T1, y: T2) = {
      if (x.numScalars > y.rank) throw new Exception() //TODO should be length error
      
      var ret = y.ravel
    	
      for ((ji, r) <- x.ravel.zip(y.rank to (y.rank - x.numScalars) by -1)) {
    	  val numAt = y.numItemsAt(r)
    	  val sizeAt= y.sizeItemAt(r)
    	  val toDrop = ji | y.rank
    	  
    	  
    	  
//    	  val itemsForThisRank = Vector() ++ (for (i <- 0 until numAt) yield {
//    	    //ret.slice(i*sizeAt, (i+1)*sizeAt)
//    	    for (j <- 0 until (numAt / y.shape(y.rank - r))) yield {
//    	      
//    	    }
//    	  })
    	  
    	  //ret = (itemsForThisRank.drop(toDrop) ++ itemsForThisRank.take(toDrop)).foldLeft(Vector[JArrayType]())(_ ++ _)
    	  
      }
      
      JArray(y.jaType, y.shape, ret)
    }
  }
}

  /*//TODO
   * rank
   * composition/bond
   * shift
   * logical and/or
   * reduce/scan(?)
   * roll
   * stitch
   * square square root
   */