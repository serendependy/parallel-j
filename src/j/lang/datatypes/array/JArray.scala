package j.lang.datatypes.array

import collection.breakOut

import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.types._
import j.lang.datatypes.array.ArrayImplicits._
import JArrayFlag._
import j.util.Rational

import JArray._
import j.lang.datatypes.array.types.JNumberTypes._
import j.lang.datatypes.JDataType

object JArray {
  
  def apply[TJ <: JArrayType : Manifest](jaType: JTypeMacro, shape: List[Int], ravel: Vector[TJ]) = {
    new JArray(afNONE, jaType, 0, shape, ravel)
  }
  
  def apply[TJ <: JArrayType : Manifest, T <% TJ](flag: JArrayFlag, jaType: JTypeMacro, refcount: Int, shape: List[Int], ravel: Vector[T]): JArray[TJ] = { //TODO LOL!
	  new JArray(flag, jaType, refcount, shape, ravel.map((t:T) => implicitly[T => TJ](implicitly[T => TJ])(t)) )
  }
  
  def apply[TJ <: JArrayType : Manifest, T <% TJ](jaType: JTypeMacro, shape: List[Int], ravel: Vector[T]):JArray[TJ] = {
    JArray(afNONE, jaType, 0, shape, ravel)
  }
  
  def auto[TJ <: JArrayType : Manifest, T <% TJ](args: T*) = {
    JArray(afNONE, args(0).jtype, 0, List(args.length), Vector(args:_*))
  }
  
  def scalar[TJ <: JArrayType : Manifest, T <% TJ](sc: T) = {
    new JArray(afNONE, sc.jtype,0, List(), Vector[TJ](sc))
  }
  
  def scalar[TJ <: JArrayType : Manifest](sc: TJ) = {
    new JArray(afNONE, sc.jtype, 0, List(), Vector(sc))
  }
    
    def arithmeticProgression(n: Int, b: Int, m: Int) =
      JArray[JInt, Int](afNONE, jINT, 0, List(n), Vector((0 to n).map(b + m * _):_*))

//    def string(str: String) = {
//      JArray(afNONE, jCHAR, 0, str.length, List(str.length), Vector(str:_*))
//    }
      
    def vec2(a: Int, b: Int)= {
      JArray[JInt, Int](afNONE, jINT, 0, List(2), Vector(a,b))
    }
    
    def vec2(a: JInt, b: JInt) = {
      new JArray(afNONE, jINT, 0, List(2), Vector(a,b))
    }
    
    def vec1(a: Int) = {
      JArray[JInt,Int](afNONE, jINT, 0, List(1), Vector(a))
    }

  val zero = scalar[JInt,Int](0)
  val one  = scalar[JInt,Int](1)
  val two  = scalar[JInt,Int](2)
  val mone = scalar[JInt,Int](-1)
  val pi   = scalar[JFloat,Double](scala.Math.Pi)
}
class JArray[+T <: JArrayType : Manifest](val flag: JArrayFlag, val jaType: JTypeMacro, 
    var refcount: Int, val shape: List[Int], 
    val ravel: Vector[T]) extends JDataType(jaType){//

  lazy val rank = shape.length
  lazy val tally = shape(0)
  lazy val itemSize = shape.drop(1).foldLeft(1)(_ * _)
  lazy val numItemz = if (shape isEmpty) 1 else shape(0)//TODO double check this doesn't break anything
  
  lazy val rankSizes = shape.scanRight(1)(_ * _).reverse
  lazy val rankItems = shape.scanLeft(1)(_ * _).reverse
  
  val numScalars = ravel.length
  
  def isScalar = shape isEmpty
  
  def numItemsAt(r: Int) = numAtRank(r-1)
  def sizeItemAt(r: Int) = sizeAtRank(r-1)
  
  def numAtRank(r: Int) = rankItems(if(r > rank) rank else r)
  def sizeAtRank(r: Int)= rankSizes(if(r > rank) rank else r)
  
    def apply(ind: Int):JArray[T] = {
     if (ind < 0) this(tally + ind) else {
        val itemShape = shape.drop(1)
        val itemSize = itemShape.foldLeft(1)(_ * _)
        val trueIndex = ind * itemSize
        //new JArray(itemShape, (ind * itemSize).until((ind+1) * itemSize).toList.map(vals))
        new JArray(flag, jaType, 0, itemShape, ravel.slice(trueIndex, trueIndex+itemSize)  )
        }
    }
  
    lazy private val sigInd = (for (i <- 0 until shape.length) yield {
      shape.drop(i).foldLeft(1)(_ * _)
    }).drop(1)
  
    override def toString = {
    (for (i <- 0 until ravel.length) yield {
      val endItem = sigInd.map((i+1) % _ == 0).zip(sigInd).filter(t => t._1).map(_._2)
      ravel(i).toString + {if (endItem.length == 0) " " else "\n" * endItem.length}
    }).mkString.dropRight(shape.length - 1)
  }
    
    //cheating
    def toJInt(implicit ev1: T <:< JFloat) = {
      JArray(jaType, shape, ravel.map((f:T) => JInt(ev1(f).v.toInt)))
    }
}
