package j.lang.primitives

import j.util.CMacroType

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.function.{JVerb, JVerb1Type, JVerb2Type}

import j.lang.datatypes.array.{JArray, JArrayType}
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.JArrayFlag._

import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.array.types._


object JVerbs {
  
  final object leftIdentity extends JVerb1Type[JArrayType](
      "[",
      List(JFuncRank(JInfinity)),
      jANY
  ){
    override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = y
    override def dyadImpl[T1 <: JArrayType : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = x  
  }

  final object rightIdentity extends JVerb1Type[JArrayType](
      "]",
      List(JFuncRank(JInfinity)),
      jANY
  )
  {
    override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = y
    override def dyadImpl[T1 <: JArrayType : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = y
  }
  //1050, 939 + utilities, 199 move in special
  final object shapeReshape extends JVerb[JArrayType, JInt, JArrayType, JInt, JArrayType](
      "$",
      List(JFuncRank(JInfinity, 1, JInfinity)),
      jANY, jINT, jANY
  ) {
    override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = 
      JArray[JInt](jINT, List(y.shape.length), Vector() ++ y.shape)
//      JArray.apply(afNONE, jINT, 0, y.rank, List[Int](y.shape.length), Vector() ++ y.shape)
      
    override def dyadImpl[T1 <: JInt : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
      val numItems: Int = x.ravel.fold(JReal.One)(_ * _)
      val ravel = Vector.tabulate(numItems)((i: Int) =>
        y.ravel(i % y.numScalars)) //TODO global fill used for !. (fit)
      JArray(y.jaType, x.ravel.toList.map(_ v), ravel)
    } //TODO modulo is slow
  }

  final object conjugatePlus extends JVerb1Type[JNumber](
      "+",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = y.ravel(0) match {
      //TODO case c: JComplex => ...
      case a:JNumber => y
    }
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) = 
      JArray(x.jaType | y.jaType, List(), Vector(x.ravel(0) + y.ravel(0)))
  }
  
  final object negateMinus extends JVerb1Type[JNumber](
      "-",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = JArray.scalar(- y.ravel(0))
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) =
      JArray.scalar(x.ravel(0) - y.ravel(0))
  }
 
  final object integersIndex extends JVerb[JInt, JArrayType, JArrayType, JInt, JInt](
      "i.",
      List(JFuncRank(1, JInfinity, JInfinity)),
      jINT, jANY, jANY
   ){

    override def monadImpl[T <: JInt : Manifest](y: JArray[T]): JArray[JInt] = 
      JArray[JInt,Int](jINT, y.ravel.toList, Vector.tabulate(y.ravel.foldLeft(1)(_ * _))((x: Int) => x))
    
    override def dyadImpl[T1 <: JArrayType : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
      throw new Exception() //TODO implement
    }
  }

  
  final object ravelAppend extends JVerb1Type[JArrayType](
      ",",
      List(JFuncRank(JInfinity)),
      jANY
  ){
    override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = {
      println("---in ravel\nArgument array is:\n")
      JArray(y.jaType, List(y.numScalars), y.ravel)
    }
    
    //TODO kill myself. Why have fancy type checking if I still have to do this?
    override def dyadImpl[T1 <: JArrayType : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = y match {
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
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = JArray(y.jaType, List(), Vector(y.ravel(0) + 1))
    override def dyadImpl[T1 <: JReal : Manifest, T2 <: JReal : Manifest](x: JArray[T1], y: JArray[T2]) = {
      JArray[JInt,Int](jINT, List(), Vector(if (x.ravel(0) >= y.ravel(0)) 1 else 0))
    }
  }
  
  final object decrementLesserthan extends JVerb[JNumber, JReal, JReal, JNumber, JInt](
      "<:",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = JArray(y.jaType, List(), Vector(y.ravel(0) - 1))
    override def dyadImpl[T1 <: JReal : Manifest, T2 <: JReal : Manifest](x: JArray[T1], y: JArray[T2]) = {
      JArray[JInt,Int](jINT, List(), Vector(if (x.ravel(0) <= y.ravel(0)) 1 else 0))
    }
//    override def dyadImpl[T1 <: JReal : Manifest, T2 <: JReal : Manifest](x: JArray[T1], y: JArray[T2]):JArray[JInt] = {
//      JArray[JInt,Int](jINT, List(), Vector(if (x.ravel(0) <= y.ravel(0)) 1 else 0))
//      /*if (x.ravel(0) <= y.ravel(0)) x else y*/
//    }
  }
  
  final object floorLesserof extends JVerb[JNumber, JReal, JReal, JNumber, JReal](
      "<.",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = y.ravel(0) match {
      case inf: JInfinite => y
      case int: JInt => y
      case r: JFloat => JArray[JInt,Int](jINT, List(), Vector(r.v.toInt))
      case _ => throw new Exception() //TODO implement
    }
    
    override def dyadImpl[T1 <: JReal : Manifest, T2 <: JReal : Manifest](x: JArray[T1], y: JArray[T2]) = {
      JArray(jNUMERIC, List(), Vector(if (x.ravel(0) < y.ravel(0)) x.ravel(0) else y.ravel(0)))
    }
  }
  
  final object ceilingGreaterof extends JVerb[JNumber, JReal, JReal, JNumber, JReal](
      ">.",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = y.ravel(0) match {
      case inf: JInfinite => y
      case int: JInt => y
      case r: JFloat => JArray[JInt,Int](jINT, List(), Vector(r.v.+(0.5).toInt))
      case _ => throw new Exception() //TODO implement
    }
    
    override def dyadImpl[T1 <: JReal : Manifest, T2 <: JReal : Manifest](x: JArray[T1], y: JArray[T2]) = {
      JArray(jNUMERIC, List(), Vector(if (x.ravel(0) < y.ravel(0)) y.ravel(0) else x.ravel(0)))
    }
  }
  
  final object naturalExponent extends JVerb1Type[JNumber](
      "^",
      List(JFuncRank(0)),
      jNUMERIC
  ) {
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) =
      JArray.scalar(JReal.E ** y.ravel(0))
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) =
      JArray.scalar(x.ravel(0) ** y.ravel(0))
  }

  final object naturalLog extends JVerb1Type[JNumber](
      "^.",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = {
      JArray.scalar(JReal.E %% y.ravel(0))
    }
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) = {
      JArray.scalar(x.ravel(0) %% y.ravel(0))
    }
  }
  
  final object reverseShift extends JVerb[JArrayType, JInt, JArrayType, JArrayType, JArrayType](
      "|.",
      List(JFuncRank(JInfinity, 1, JInfinity)),
      jANY, jINT, jANY
  ){
    override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = {
      JArray(y.jaType, y.shape,
          (0 until y.numItemz).reverse.map(
              i => y.ravel.slice(i, i*y.itemSize)).foldLeft(
                  Vector[JArrayType]())(_ ++ _))
    }
    
    override def dyadImpl[T1 <: JInt : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
      println("---in reverseShift, x is: " + x)
      println("                  , y is: " + y)
      if (x.numScalars > y.rank) throw new Exception() //TODO should be length error
      
      var ret = y.ravel
    	
      for ((shift, r) <- x.ravel.zip(y.rank to (y.rank - x.numScalars) by -1)) {
    	  val numAt = y.numItemsAt(r)
    	  val sizeAt= y.sizeItemAt(r)
    	  val toDrop = r % y.shape(r-1)
    	  
    	  val distinctionsAtThisRank = Vector() ++ (for (i <- 0 until y.numAtRank(r)) yield {
    	    val start = i * y.sizeAtRank(r)
    	    Vector() ++ (for (j <- 0 until (y.numItemsAt(r) / y.numAtRank(r))) yield {
    	    	ret.slice(start + (j * sizeAt), start + (j+1)*sizeAt)
    	    })
    	  })
    	  
    	  ret = distinctionsAtThisRank.map(x => (x.drop(toDrop) ++ x.take(toDrop)).foldLeft(
    	      Vector[T2]())(_ ++ _)).foldLeft(
    	          Vector[T2]())(_ ++ _)
      }
      
      JArray(y.jaType, y.shape, ret)
    }
  }
  
  final object realOr extends JVerb[JNumber, JNumber, JNumber, JReal, JNumber](
      "+.",
      List(JFuncRank(0)),
      jNUMERIC, jNUMERIC, jNUMERIC
   ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = y match {
      case r: JArray[JReal] => r
      case _ => throw new Exception() //TODO implement for complex numbers
    }
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) = {
      if ((x.ravel(0) != JReal.Zero) || (y.ravel(0) != JReal.Zero))  JArray.scalar(JReal.One) else JArray.scalar(JReal.Zero)
    }//TODO implement GCD
  }
  
  final object lengthangleAnd extends JVerb1Type[JNumber](
      "*.",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = {
      throw new Exception()//TODO implement
    }
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) = {
      if ((x.ravel(0) != JReal.Zero) && (y.ravel(0) != JReal.Zero)) JArray.scalar(JReal.One) else JArray.scalar(JReal.Zero)
    }
  }
  
  final object rollDeal extends JVerb1Type[JInt](
      "?",
      List(JFuncRank(0)),
      jINT
  ){
    override def monadImpl[T <: JInt : Manifest](y: JArray[T]) = {
      JArray.scalar[JInt](scala.util.Random.nextInt(y.ravel(0)))
    }
    
    override def dyadImpl[T1 <: JInt : Manifest, T2 <: JInt : Manifest](x: JArray[T1], y: JArray[T2]) = {//TODO optimize
      JArray(jINT, List(x.ravel(0)),
          scala.util.Random.shuffle(Vector.tabulate[JInt](
              y.ravel(0))((x: Int) => x)).drop(y.ravel(0) - x.ravel(0)))
    }
  }	
  
  final object squareNotand extends JVerb1Type[JNumber](
      "*:",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = {
      JArray.scalar(y.ravel(0) ** 2)
    }
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) = {
      if ((x.ravel(0) != JReal.Zero) && (y.ravel(0) != JReal.Zero)) JArray.scalar(JReal.Zero) else JArray.scalar(JReal.One)
    }
  }
  
  final object squarerootRoot extends JVerb1Type[JNumber](
      "%:",
      List(JFuncRank(0)),
      jNUMERIC
  ){
    override def monadImpl[T <: JNumber : Manifest](y: JArray[T]) = {
      JArray.scalar(y.ravel(0) ** 0.5)
    }
    
    override def dyadImpl[T1 <: JNumber : Manifest, T2 <: JNumber : Manifest](x: JArray[T1], y: JArray[T2]) = {
      JArray.scalar(y.ravel(0) ** x.ravel(0).inv)
    }
  }
 
  val ravelitemsStitch = ravelAppend.addRanks(JFuncRank(-1))

}

  /*//TODO
   * composition/bond
   * scan(?)
   */