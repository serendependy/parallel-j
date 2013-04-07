package j.test.benchmark.GameOfLife

import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.datatypes.array.ArrayImplicits._
import j.lang.datatypes.array.JArray

object GOLParams {
	val boardShape = JArray.auto[JInt,Int](100,100)
	val steps = 10
	val ratioAliveDead = new JFloat(0.5)
}
