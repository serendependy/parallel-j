package j.test.benchmark.MergeSort

/*Imports*/

abstract class MergeSortBench extends Benchmark {

    var y: JArray[JInt] = null
    var res: JArray[JInt] = null    
  
     override def main(args: Array[String]) {
        //command line argument parsing, setting value for y
    }
    
    override def setUp()
    
    def run() {
      val jtwo = JArray.scalar(JInt(2))
      val j16  = JArray.scalar(JInt(16))

      val intReverse = reverseShift.asInstanceOf[
        JVerb[JInt, JInt, JInt, JInt, JInt]]

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
