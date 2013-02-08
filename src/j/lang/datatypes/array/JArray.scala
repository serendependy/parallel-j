package j.lang.datatypes.array

import j.lang.datatypes.JTypeMacros._
import j.lang.datatypes.array.types._
import j.lang.datatypes.array.ArrayImplicits._
import JArrayFlag._
import j.util.Rational

import JArray._
import j.lang.datatypes.array.types.JNumberTypes._

object JArray {
  
  def apply[T <% JArrayType : Manifest](flag: JArrayFlag, jaType: JType, refcount: Int, 
      numItems: Int, shape: List[Int], ravel: Array[T]) = 
        new JArray(flag, jaType, refcount, numItems, shape, ravel)
  
  def apply[T <% JArrayType : Manifest](jaType: JType, shape: List[Int], ravel: Array[T]) = {
    new JArray(afNONE, jaType, 0, ravel.length, shape, ravel)
  }
  
  def auto[T <% JArrayType : Manifest](args: T*) = {
    new JArray(afNONE, args(0).typeMacro, 0, args.length, List(args.length), args.toArray)
  }
  
  def scalar[T <% JArrayType : Manifest](sc: T): JArray[T] = {
    val flag = afNONE
    val jaType = sc.typeMacro
    val shape = List[Int]()
    val ravel = Array[T](sc)
    JArray(afNONE, sc.typeMacro,0, 1, List(), Array[T](sc))
  }
    
    def arithmeticProgression(n: Int, b: Int, m: Int) =
      JArray(afNONE, jINT, 0, n, List(n), (0 to n).map(b + m * _).toArray)

    def string(str: String) = {
      JArray(afNONE, jCHAR, 0, str.length, List(str.length), str.toCharArray())
    }
      
    def vec2(a: Int, b: Int)= {
      JArray(afNONE, jINT, 0, 2, List(2), Array(a,b))
    }
    
    def vec1(a: Int) = {
      JArray(afNONE, jINT, 0, 1, List(1), Array(a))
    }

  val zero = scalar(0)
  val one  = scalar(1)
  val two  = scalar(2)
  val mone = scalar(-1)
  val pi   = scalar(scala.Math.Pi)
}
class JArray[+T <% JArrayType : Manifest](val flag: JArrayFlag, val jaType: JType, 
    var refcount: Int, val numItems: Int, val shape: List[Int], 
    val ravel: Array[T]) {//

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
  
    lazy private val sigInd = (for (i <- 0 until shape.length) yield {
      shape.drop(i).reduceLeft(_ * _)
    }).drop(1)
  
    override def toString = {
    (for (i <- 0 until ravel.length) yield {
      val endItem = sigInd.map((i+1) % _ == 0).zip(sigInd).filter(t => t._1).map(_._2)
      ravel(i).toString + {if (endItem.length == 0) " " else "\n" * endItem.length}
    }).mkString.dropRight(shape.length - 1)
  }
}
