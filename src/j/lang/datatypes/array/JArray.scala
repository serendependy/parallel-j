package j.lang.datatypes.array

import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.types._
import j.lang.datatypes.array.types.TypeImplicits._
import JArrayFlag._
import j.util.Rational

import JArray._
import j.lang.datatypes.array.types.JNumber

object JArray {
  
  def apply[T <% JArrayType : Manifest](flag: JArrayFlag, jaType: JType, refcount: Int, 
      numItems: Int, shape: List[Int], ravel: Array[T]) = 
        new JArray(flag, jaType, refcount, numItems, shape, ravel)
  
  def scalar[T <% JArrayType : Manifest](sc: T): JArray[T] = {
    JArray(afNONE, sc.typeMacro,0, 1, List(), Array[T](sc))
  }
    
    def arithmeticProgression(n: Int, b: Int, m: Int):JArray[JInt] =
      JArray(afNONE, jINT, 0, n, List(n), (0 to n).map(b + m * _).toArray)

    def string(str: String) = {
      JArray(afNONE, jCHAR, 0, str.length, List(str.length), str.toCharArray())
    }
      
    def vec2(a: Int, b: Int):JArray[JInt] = {
      JArray(afNONE, jINT, 0, 2, List(2), Array(a,b))
    }
    
    def vec1(a: Int):JArray[JInt] = {
      JArray(afNONE, jINT, 0, 1, List(1), Array(a))
    }

  val zero = scalar(0)
  val one  = scalar(1)
  val two  = scalar(2)
  val mone = scalar(-1)
  val pi   = scalar(scala.Math.Pi)
}
class JArray[T <% JArrayType : Manifest](val flag: JArrayFlag, val jaType: JType, 
    var refcount: Int, val numItems: Int, val shape: List[Int], 
    val ravel: Array[T]) {

  def rank = shape.length
  def tally = shape(0)
  
  def isScalar = shape isEmpty
  
    def apply(ind: Int):JArray[T] = {
     if (ind < 0) this(tally + ind) else {
        val itemShape = shape.drop(1)
        val itemSize = itemShape.reduceLeft(_ * _)
        val trueIndex = ind * itemSize
        //new JArray(itemShape, (ind * itemSize).until((ind+1) * itemSize).toList.map(vals))
        new JArray(flag, jaType, 0, itemSize, itemShape, (trueIndex).until(trueIndex + itemSize).
            map(ravel).toArray  )
        }
    } 
}