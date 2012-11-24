package j.lang.datatypes

import j.lang.datatypes.JArrayType._
import j.lang.datatypes.JArrayFlag._

import j.util.Rational

object JArray {
  
  def apply[T: JArrayType](flag: JArrayFlag, jaType: JAType, refcount: Int, 
      numItems: Int, shape: List[Int], ravel: Array[T]) = 
        new JArray(flag, jaType, refcount, numItems, shape, ravel)
  
  def scalar[T: JArrayType](sc: T) = {
    sc match { // should have exhaustiveness check, but doesn't b/c nesting
      case x:Byte	=> JArray(afNONE, jB01, 0, 1, List(), Array(x))
      case x:Int 	=> JArray(afNONE, jINT,	0, 1, List(), Array(x))
      case x:Double	=> JArray(afNONE, jFL,	0, 1, List(), Array(x))
      case x:Char	=> JArray(afNONE, jCHAR,0, 1, List(), Array(x))
      case x:Rational=>JArray(afNONE, jXNUM,0, 1, List(), Array(x))
    }
    
    def arithmeticProgression(n: Int, b: Int, m: Int) =
      JArray(afNONE, jINT, 0, n, List(n), (0 to n).map(b + m * _).toArray)

    def string(str: String) = {
      JArray(afNONE, jCHAR, 0, str.length, List(str.length), str.toCharArray())
    }
      
    def vec2(a: Int, b: Int) = {
      JArray(afNONE, jINT, 0, 2, List(2), Array(a,b))
    }
    
    def vec1(a: Int) = {
      JArray(afNONE, jINT, 0, 1, List(1), Array(a))
    }
  }
  
  
  val zero = scalar(0)
  val one  = scalar(1)
  val two  = scalar(2)
  val mone = scalar(-1)
  val pi   = scalar(scala.Math.Pi)
}

import JArray._
class JArray[T: JArrayType](val flag: JArrayFlag, val jaType: JAType, 
    var refcount: Int, val numItems: Int, val shape: List[Int], 
    val ravel: Array[T]) {

  
  
  val rank = shape.length
  def tally = shape(0)
}