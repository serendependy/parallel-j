package j.test.benchmark.MergeSort

import scala.testing.Benchmark
import scala.compat.Platform
import collection.parallel.ForkJoinTasks.defaultForkJoinPool

import j.lang.datatypes.JFuncRank

import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.datatypes.function.JVerb
import j.lang.datatypes.function.JVerb1Type

import j.lang.primitives.JVerbs._
import j.lang.primitives.cheating.JCheating._

abstract class MergeSortBench extends Benchmark {

	var y: JArray[JInt] = null
	var res: JArray[JInt] = null	
  
	 override def main(args: Array[String]) {
  	  if (args.length >= 2) {
  		  val logFile = new java.io.OutputStreamWriter(System.out)
  		  logFile.write(prefix)
  		  val numToDeal = JArray.scalar[JInt,Int](1 << args(0).toInt)
  		  y = rollDeal(numToDeal, numToDeal)
  		  if (args.length >= 3)
  			  defaultForkJoinPool.setParallelism(args(2).toInt)
  		  for (t <- runBenchmark(args(1).toInt))
  			logFile.write("\t" + t)

  		  logFile.write(Platform.EOL)
  		  logFile.flush()
      } 
  	  else {
  		  println("Usage: scala j.test.benchmark.MergeSort.MergeSortBench_<Seq|Par> <size> <runs> ")
  	  }
  	}
	
	override def setUp()
	
	def run() {
     val jtwo = JArray.scalar(JInt(2))
      val j16  = JArray.scalar(JInt(16))

      val intReverse = reverseShift.asInstanceOf[JVerb[JInt, JInt, JInt, JInt, JInt]]
      val sort2 = (decrementLesserthanequal insert) agenda(
          reverse,
          rightIdentity.asInstanceOf[JVerb1Type[JInt]])

      val divide = (y: JArray[JInt]) => {
        shapeReshape(
            tallyCopies(
                naturalLog(
                    jtwo,
                    tallyCopies(y)).asInstanceOf[JArray[JFloat]].toJInt,
                jtwo).asInstanceOf[JArray[JInt]],
            y).asInstanceOf[JArray[JInt]]
      }

      val dim = (y: JArray[JInt]) => {
        tallyCopies(shapeReshape(y))
      }

      val sortBase = (sort2 addRanks(JFuncRank(1)) )
      val merger2 = (merge insert) addRanks(JFuncRank(2))

      val repeatedMerge = (y: JArray[JInt]) => {
        merger2.power(decrementLesserthanequal(dim(y)
            ).asInstanceOf[JArray[JInt]]).apply(y)
      }

      def mergeSort = (y: JArray[JInt]) => {
        repeatedMerge(sortBase(divide(y)))
      }

	  res = mergeSort(y)	  
	}
	
	override def tearDown() {
	  println("Array is sorted: " + 
	      (decrementLesserthanequal insert).apply(res) )
	  super.tearDown()
	}
}