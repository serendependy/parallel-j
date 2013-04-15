package j.test.benchmark.GameOfLife

/*Imports*/

abstract class GOLBench extends Benchmark {
    var boardShape: JArray[JInt] = null
    val steps = 10
    val ratioAliveDead = new JFloat(0.5)
    
    //helper verbs
    val equals3 = new JVerb1Type[JInt](
        "(3 = ])",
        List(JFuncRank(0)),
        jINT){
        override def monadImpl[T <: JArrayType : Manifest](
            y: JArray[T]) = 
                throw new NotImplementedException()

        override def dyadImpl[T1 <: JArrayType : Manifest, 
            T2 <: JArrayType : Manifest](
            x: JArray[T1], y: JArray[T2]) = {
                equal(JArray.scalar[JInt,Int](3),
                      y)
            }
          }
    val equals2_or_3 = new JVerb1Type[JInt](
          "([: +./ 2 3 = ])",
          List(JFuncRank(0)),
          jINT){
            override def monadImpl[T <: JInt : Manifest](
                y: JArray[T]) = 
                    throw new NotImplementedException()

            override def dyadImpl[T1 <: JInt : Manifest, 
                T2 <: JArrayType : Manifest](
                x: JArray[T1], y: JArray[T2]) = {
                    (realOr insert).apply(equal(
                              JArray.vec2(2,3),
                               y)).asInstanceOf[JArray[JInt]]
            }
          }
    
    override def main(args: Array[String]) {
        //command line argumen parsing, setting value for boardShape
    }
    
    override def setUp()
    
    def run() {
    
      val numCells = (signumMultiply insert).monad(
            boardShape).asInstanceOf[JArray[JInt]]

      val lifeThreshold = 
        signumMultiply(
            JArray.scalar(ratioAliveDead),
            numCells).asInstanceOf[JArray[JReal]]

      val board = shapeReshape(
          boardShape,
          incrementGreaterthanequal(
              lifeThreshold,
              rollDeal[JArray[JInt],JArray[JInt]](
                  numCells,
                  numCells))).asInstanceOf[JArray[JInt]]

      val shiftBy = shapeReshape(
          JArray.vec2(8, 2),//    DR      D      DL   
          JArray.auto[JInt, Int](
          //DR      D      DL 
           -1,-1,  -1,0,  -1,1,  

          //R      L     UR     U     UL
            0,-1,  0,1,  1,-1,  1,0,  1,1)
          ).asInstanceOf[JArray[JInt]]

      val neighborArray = (y: JArray[JInt]) => 
            reverseShift(shiftBy, y).asInstanceOf[JArray[JInt]]

      val listNeighbors = (y: JArray[JInt]) => 
            (conjugatePlus insert).monad(neighborArray(y)
            ).asInstanceOf[JArray[JInt]]

      val nextState = leftIdentity.asInstanceOf[
            JVerb[JInt, JInt, JInt, JInt, JInt]] agenda(
          equals3, equals2_or_3)

      var boardvar = board

      for (i <- 0 until steps) {
        boardvar = 
            nextState(boardvar, listNeighbors(boardvar)
            ).asInstanceOf[JArray[JInt]]
      }   
    }
}
