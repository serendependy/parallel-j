package j.test.benchmark.MergeSort

import scala.testing.Benchmark

import j.lang.datatypes.JTypeMacros
import j.lang.datatypes.JFuncRank

import j.lang.datatypes.function.JVerb
import j.lang.datatypes.function.JVerb1Type

import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.primitives.JVerbs._

import j.lang.primitives.cheating.JCheating._

object MergeSortBench_Seq extends Benchmark {
  import j.test.benchmark.GameOfLife.GOLParams._
  def run() {
    JVerb.parallelFlag = false
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

	  mergeSort(y)
  }
}
