package j.lang.datatypes

import j.lang.datatypes.JArrayType._
import j.lang.datatypes.JArrayFlag._

object JArray {
  
  def apply[T: JArrayType](flag: JArrayFlag, jaType: JAType, refcount: Int, numItems: Int,
      shape: List[Int], ravel: Array[T]) = 
        new JArray(flag, jaType, refcount, numItems, shape, ravel)
  
  def scalar(k: Int) = JArray(afNONE, jINT, 0, 1, List(), Array(k) )
  
  val test = scalar(0)
}

import JArray._
class JArray[T: JArrayType](val flag: JArrayFlag, val jaType: JAType, var refcount: Int, 
    val numItems: Int, val shape: List[Int], val ravel: Array[T]) {
  val rank = shape.length
  def tally = shape(0)
}