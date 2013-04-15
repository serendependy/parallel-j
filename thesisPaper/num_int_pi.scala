package j.test.benchmark.NumInt

/*Imports*/

abstract class NumIntBench extends Benchmark {
  
    var numSquares: JInt = null
    private var pi: JArray[JNumber] = null
  
    override def setUp()
    
    //modified from scala.testing.Benchmark
    override def main(args: Array[String]) {
        //command line argument parsing, setting value for numSquares
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
