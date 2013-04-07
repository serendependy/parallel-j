package j.test.benchmark.NumInt

import scala.testing.Benchmark

import j.lang.datatypes.function.JVerb

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.primitives.JVerbs._

object NumIntPar_Bench extends Benchmark {
  import j.test.benchmark.NumInt.NumIntParams._
  
  def run() {
    JVerb.parallelFlag = true
    
      val recip = JArray.scalar(numSquares.recip)
      val xvals =  signumMultiply(integersIndex(JArray.scalar(numSquares)),
    		  					  recip)
      val yvals = recipricalDivide(
          conjugatePlus(JArray.scalar(JReal.One)), 
    		  squareNotand(xvals))

      val pi = signumMultiply(
          signumMultiply(
              (conjugatePlus insert).apply(yvals),
              JArray.scalar[JInt,Int](4)),
          recip) 
  }
}