package j.test.benchmark.NumInt

import scala.testing.Benchmark
import scala.compat.Platform
import collection.parallel.ForkJoinTasks.defaultForkJoinPool

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.primitives.JVerbs._
import j.lang.primitives.cheating.JCheating._

abstract class NumIntBench extends Benchmark {
  
  	var numSquares: JInt = null
	private var pi: JArray[JNumber] = null
  
	override def setUp()
	
  	//modified from scala.testing.Benchmark
  	override def main(args: Array[String]) {
  	  if (args.length >= 2) {
  		  val logFile = new java.io.OutputStreamWriter(System.out)
  		  logFile.write(prefix)
  		  numSquares = args(0).toInt 
  		  if (args.length >= 3)
  			  defaultForkJoinPool.setParallelism(args(2).toInt)
  		  for (t <- runBenchmark(args(1).toInt))
  			logFile.write("\t" + t)

  		  logFile.write(Platform.EOL)
  		  logFile.flush()
      } 
  	  else {
  		  println("Usage: scala j.test.benchmark.NumInt.NumIntBench_<Seq|Par> <size> <runs> ")
  	  }
  	}
  	
	def run() {
	  val recip = JArray.scalar(numSquares.recip)
      val xvals =  signumMultiply(integersIndex(JArray.scalar(numSquares)),
    		  					  recip)
      val yvals = recipricalDivide(
          conjugatePlus(JArray.scalar(JReal.One), 
    		  squareNotand(xvals)))

      pi = (conjugatePlus insert).apply(signumMultiply(
          yvals, 
          signumMultiply(
              JArray.scalar[JInt,Int](4),
              recip)
          ))
	}
	
	override def tearDown() {
	  println("Pi is " + pi)
	  
	  super.tearDown()
	}
}