package j.test.benchmark.NumInt

import scala.testing.Benchmark

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.primitives.JVerbs._
import j.lang.primitives.cheating.JCheating._

abstract class NumIntBench extends Benchmark {
  
  	val numSquares = JInt(20000)
	private var pi: JArray[JNumber] = null
  
	override def setUp()
	
	def run() {
	  val recip = JArray.scalar(numSquares.recip)
      val xvals =  signumMultiply(integersIndex(JArray.scalar(numSquares)),
    		  					  recip)
      val yvals = recipricalDivide(
          conjugatePlus(JArray.scalar(JReal.One)), 
    		  squareNotand(xvals))

      pi = signumMultiply(
          signumMultiply(
              (conjugatePlus insert).apply(yvals),
              JArray.scalar[JInt,Int](4)),
          recip)
	}
	
	override def tearDown() {
	  println("Pi is " + pi)
	}
}