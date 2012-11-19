package j.lang.datatypes

import j.lang.datatypes.JArrayType._
import j.lang.datatypes.JArrayFlag._

object JArray {}

import JArray._
class JArray[A](val flag: JArrayFlag, val jaType: JAType, val refcount: Int, 
    val numItems: Int, val shape: List[Int], val ravel: Array[A]) {
  val rank = shape.length
}