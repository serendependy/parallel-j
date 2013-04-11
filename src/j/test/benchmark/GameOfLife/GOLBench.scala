package j.test.benchmark.GameOfLife

import scala.testing.Benchmark
import scala.compat.Platform
import collection.parallel.ForkJoinTasks.defaultForkJoinPool
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros._

import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.datatypes.function.JVerb
import j.lang.datatypes.function.JVerb1Type

import j.lang.primitives.JVerbs._
import j.lang.primitives.cheating.JCheating._

abstract class GOLBench extends Benchmark {
	var boardShape: JArray[JInt] = null
	val steps = 10
	val ratioAliveDead = new JFloat(0.5)
	
	//helper verbs
	val equals3 = new JVerb1Type[JInt](
		"(3 = ])",
        List(JFuncRank(0)),
        jINT){
        override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = throw new NotImplementedException()

        override def dyadImpl[T1 <: JArrayType : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
        	equal(JArray.scalar[JInt,Int](3),
                  y)
            }
          }
	val equals2_or_3 = new JVerb1Type[JInt](
          "([: +./ 2 3 = ])",
          List(JFuncRank(0)),
          jINT){
            override def monadImpl[T <: JInt : Manifest](y: JArray[T]) = throw new NotImplementedException()

            override def dyadImpl[T1 <: JInt : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
              (realOr insert).apply(equal(
                                    JArray.vec2(2,3),
                                    y)).asInstanceOf[JArray[JInt]]
            }
          }
	
 	override def main(args: Array[String]) {
  	  if (args.length >= 2) {
  		  val logFile = new java.io.OutputStreamWriter(System.out)
  		  logFile.write(prefix)
  		  boardShape = JArray.auto[JInt,Int](args(0).toInt, args(0).toInt)
  		  if (args.length >= 3)
  			  defaultForkJoinPool.setParallelism(args(2).toInt)
  		  for (t <- runBenchmark(args(1).toInt))
  			logFile.write("\t" + t)

  		  logFile.write(Platform.EOL)
  		  logFile.flush()
      } 
  	  else {
  		  println("Usage: scala j.test.benchmark.GameOfLife.GOLBench_<Seq|Par> <size> <runs> ")
  	  }
  	}
	
	override def setUp()
	
	def run() {
    
      val numCells = (signumMultiply insert).monad(boardShape).asInstanceOf[JArray[JInt]]
      val lifeThreshold = signumMultiply(JArray.scalar(ratioAliveDead),
                                         numCells).asInstanceOf[JArray[JReal]]
      val board = shapeReshape(
          boardShape,
          incrementGreaterthanequal(
              lifeThreshold,
              rollDeal[JArray[JInt],JArray[JInt]](
                  numCells,
                  numCells))).asInstanceOf[JArray[JInt]]

      val shiftBy = shapeReshape(
          JArray.vec2(8, 2),//    DR      D      DL    R      L     UR     UL
          JArray.auto[JInt, Int](-1,-1,  -1,0,  -1,1,  0,-1,  0,1,  1,-1,  1,0,  1,1)).asInstanceOf[JArray[JInt]]

      val neighborArray = (y: JArray[JInt]) => reverseShift(shiftBy, y).asInstanceOf[JArray[JInt]]
      val listNeighbors = (y: JArray[JInt]) => (conjugatePlus insert).monad(neighborArray(y)).asInstanceOf[JArray[JInt]]

      val nextState = leftIdentity.asInstanceOf[JVerb[JInt, JInt, JInt, JInt, JInt]] agenda(
          equals3, equals2_or_3)

      var boardvar = board

      for (i <- 0 until steps) {
        boardvar = nextState(boardvar, listNeighbors(boardvar)).asInstanceOf[JArray[JInt]]
      }	  
	}
	
	override def tearDown() {
		super.tearDown()
	}
}