package j.test

import j.lang.datatypes.array.types.JNumberTypes._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object Tester {
  
  def main(args: Array[String]) {
    {
     import ArrayTester._
     import j.lang.datatypes.array.JArray
     import j.lang.datatypes.array.ArrayImplicits._
     
     import j.lang.datatypes.JTypeMacros._
     
     testArraysPrint()
     val jarnum = JArray[JInt,Int](jINT, List(5,5), Vector.tabulate(5,5)((x: Int, y: Int) => if (x == y) 1 else 0).flatten)
     testArrayFrames(jarnum)
     testAFShapeTo()
     testNumAndSizes()
    }
    
    {
      import FunctionTester._
      
      testIntegers()
      testNegate()
      testRoll()
      testSquare()
      testRavelItems()
      //TODO test reverse
      
      testPlus()
      testShift()
      testDeal()
      testLogical()
      testRoot()
      testCopies()
    }
	 
    {
    	import ExampleProblems._
    	import j.lang.datatypes.array.ArrayImplicits._
    	import j.lang.datatypes.array.JArray
    	
    	testNumericalIntegration(10)
    	testGameOfLife(5,5,0.5, 10)
    	testMergeSort(JArray.auto[JInt, Int](-100, 5, 100, 10, -3, 8, 9, 1))
    }
    
  }
  
  object ExampleProblems {
    import j.lang.datatypes.array.JArray
    import j.lang.datatypes.array.JArrayType
    import j.lang.datatypes.array.ArrayImplicits._
    
    import j.lang.primitives.JVerbs._
    import j.lang.primitives.cheating.JCheating._
    
    import j.lang.datatypes.JTypeMacros._
    
    import j.lang.datatypes.function.JVerb1Type
    import j.lang.datatypes.function.JVerb
    import j.lang.datatypes.JFuncRank
    
    def testNumericalIntegration(numRiemann: JInt) {
      println("\n--Testing Numerical Integration")
      val recip = JArray.scalar(numRiemann.recip)
      val xvals =  signumMultiply(integersIndex(JArray.scalar(numRiemann)),
    		  					  recip)
      val yvals = squarerootRoot(negateMinus(JArray.scalar(JReal.One),
    		  								 squareNotand(xvals)))
      val pi = signumMultiply(
          signumMultiply(
              (conjugatePlus insert).apply(yvals),
              JArray.scalar[JInt,Int](4)),
          recip)
      
      println("PI is: " + pi)
      println("DONE")
    }
    
    def testGameOfLife(x: JInt, y: JInt, ratioAliveDead: JFloat, steps: Int) = {
      println("\n--Testing Game of Life")
      val boardShape = JArray.vec2(x, y)
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
          JArray.vec2(8, 2),//    DR	  D		 DL	   R      L     UR     UL
          JArray.auto[JInt, Int](-1,-1,  -1,0,  -1,1,  0,-1,  0,1,  1,-1,  1,0,  1,1)).asInstanceOf[JArray[JInt]]
      
      val neighborArray = (y: JArray[JInt]) => reverseShift(shiftBy, y).asInstanceOf[JArray[JInt]]
      val listNeighbors = (y: JArray[JInt]) => (conjugatePlus insert).monad(neighborArray(y)).asInstanceOf[JArray[JInt]]

      println("Board:\n" + board)
      println("Neighbor higher array:\n" + neighborArray(board))
      println("Neighbor list of board:\n" + listNeighbors(board))
   
      val nextState = leftIdentity.asInstanceOf[JVerb[JInt, JInt, JInt, JInt, JInt]] agenda(
          new JVerb1Type[JInt](
              "(3 = ])",
              List(JFuncRank(0)),
              jINT){
            override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = throw new NotImplementedException()
            
            override def dyadImpl[T1 <: JArrayType : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
            	equal(JArray.scalar[JInt,Int](3),
            	      y)
            }
          },
          
      new JVerb1Type[JInt](
          "([: +./ 2 3 = ])",
          List(JFuncRank(0)),
          jINT){
            override def monadImpl[T <: JInt : Manifest](y: JArray[T]) = throw new NotImplementedException()
            
            override def dyadImpl[T1 <: JInt : Manifest, T2 <: JArrayType : Manifest](x: JArray[T1], y: JArray[T2]) = {
              (realOr insert).apply(equal(
            		  				JArray.vec2(2,3),
            		  				y)).asInstanceOf[JArray[JInt]]
            }            
          })
      
      println("board:\n" + board)
      println("next :\n" + nextState(board, listNeighbors(board)).asInstanceOf[JArray[JInt]])
      
      var boardvar = board
      
      for (i <- 0 until steps) {
        println("step " + ":\n" + boardvar)
        boardvar = nextState(boardvar, listNeighbors(boardvar)).asInstanceOf[JArray[JInt]]
      }
      
      println("DONE")
    }
    
    
    def testMergeSort(y: JArray[JInt]) {
      println("\n--Testing MergeSort")
      
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

      println("sorting: " + y) 
      println("sorted:  " + repeatedMerge(sortBase(divide(y))))
       
       println("DONE")
    }
  }
  
  object FunctionTester {
    import j.lang.datatypes.JTypeMacros._
    
    import j.lang.datatypes.array.JArray
    import j.lang.datatypes.array.JArrayType
    import j.lang.datatypes.array.JArrayFrame
    import j.lang.datatypes.array.ArrayImplicits._
    
    import j.lang.datatypes.array.types.JNumberTypes._
    
    import j.lang.primitives.JVerbs._
    
    import j.lang.datatypes.JFuncRank
    
    def testIntegers() {
      println("\n--Testing integers")
      val r3s232 = integersIndex.monad(JArray.auto[JInt, Int](2,3,2))
      println(r3s232.shape + "\n" + (r3s232.ravel.map(_ v).mkString(" ")))
      println(r3s232)
      println("Done")
    }
    
    def testIdentity() {
      val x = JArray.auto[JInt, Int](2,3,2)
      val y = JArray.auto[JFloat, Double](2.2, 3.3, 2.2)
      val idenr = rightIdentity.monad(x)
      val idenl = leftIdentity.monad(x)
    }
    
    def testNegate() {
      println("\n--Testing Negate")
      val ar = integersIndex.monad(JArray.auto[JInt,Int](2,3,2))
      println(ar)
      val res = negateMinus.monad(ar)
      println("\n" + res)
      println("Done")
    }
    
    def testRoll() {
      println("\n--Testing Roll")
      for (i <- 10 to 20)
        println(rollDeal.monad(JArray.scalar[JInt](i)))
        
      println("Done")
    }
    
    def testSquare() {
      println("\n--Testing Square")
      for (i <- 0 to 10)
        println(squareNotand.monad(JArray.scalar[JInt](i)))
      println("Done")
    }
    
    def testRavelItems() {
      println("\n--Testing ravelItems")
      val arr3 = integersIndex.monad(JArray.auto[JInt, Int](2,3,2))
      println("arr3:\n" + arr3)
      
      println(ravelitemsStitch.monad(arr3))
      
      println("DONE")
    }
    
    //dyads
    def testPlus() {
      println("\n--Testing plus")
      val jar1 = JArray.auto[JInt, Int](1)
      val jarRes= conjugatePlus.dyad(jar1, jar1)
      println(jarRes)
      
      val vec = integersIndex.monad(JArray.auto[JInt, Int](2))
      val mat = integersIndex.monad(JArray.auto[JInt, Int](2,3))
      println("vec: " + vec)
      println("mat:\n" + mat)
      val (xframed, yframed) = JArrayFrame.createFrames(conjugatePlus.ranks, vec, mat)
      println("xframed: " + xframed)
      println("yframed: " + yframed)
      val sa = xframed.shapeAgreement(yframed).get
      println("Shape Agreement: " + sa)
      println("xreshaped: " + xframed.shapeToNewFrame(sa))
      println("yreshaped:\n" + yframed.shapeToNewFrame(sa))
      
      val vmRes = conjugatePlus.dyad(vec, mat)
      println("vec + matrix:\n" + vmRes + "\nShape: " + vmRes.shape + "\nRavel: " + vmRes.ravel)
      println("---Testing associativity")
      println(conjugatePlus.dyad(mat, vec))
      
      val edges = JArray.auto[JNumber,JNumber](JNegativeInfinity, JNumber(-1), JNumber(0), JNumber(1), JInfinity)
      val arr1  = integersIndex.monad(JArray.auto[JInt, Int](5,3,2))
      val edgeRes = conjugatePlus.dyad(edges, arr1)
      println("Testing edgeCases with infinities, signs\n" + edgeRes)
      
      println("Done")
    }
    
    def testShift() {
      println("\n--Testing shift")
      
      val arr3 = integersIndex.monad(JArray.auto[JInt, Int](2,3,2))
      println("Original Vector:\n" + arr3)
      
      val sh1  = reverseShift.dyad(JArray.auto[JInt, Int](1), arr3)
      val sh2  = reverseShift.dyad(JArray.auto[JInt, Int](1,2), arr3)
      val sh3  = reverseShift.dyad(JArray.auto[JInt, Int](1,2,1), arr3)
      val sh4  = reverseShift(JArray.auto[JInt, Int](-1, -2, -1), arr3)
      
      println(arr3 + "\n--")
      println(sh1 + "\n--")
      println(sh2 + "\n--")
      println(sh3 + "\n--")
      println(sh4 + "\n--")
      
      println("Done")
    }
    
    def testLogical() {
      println("\n--Testing Logical")
      val tr = JArray.scalar(JReal.One)
      val fa = JArray.scalar(JReal.Zero)
      val logics = List(fa, tr)
      
      println("TRUE: " + tr)
      println("FALSE: " + fa)
      
      println("OR:\n")
      for (b1 <- logics) {
        for (b2 <- logics) {
          println(realOr.dyad(b1, b2))
        }
      }

      println("AND:\n")
      for (b1 <- logics) {
        for (b2 <- logics) {
          println(lengthangleAnd.dyad(b1, b2))
        }
      }
      
      println("Done")
    }
    
    def testDeal() {
      println("--Testing Deal")
      for (i <- 0 to 10)
        println(rollDeal.dyad(JArray.scalar[JInt](i), JArray.scalar[JInt](10)))
      println("Done")
    }
    
    def testRoot() {
      println("\n--Testing Root")
      
      
      for (i <- 0 to 4) {
        for (j <- List(0,1,2,4,8,9,64,65,100)) {
          print(squarerootRoot.dyad(JArray.scalar[JInt](i), JArray.scalar[JNumber](j)))
        }
        println()
      }
      
      println("DONE")
    }
    
    def testCopies() {
      println("\n--Testing Copies")
      val choose = JArray.auto[JInt,Int](0, 1, 0, 2, 0, 1, 2, 0, 0, 3, 1, 2)
      val t1 = integersIndex(JArray.scalar[JInt,Int](12))
      val t2 = integersIndex(JArray.auto[JInt, Int](12, 3))
      
      println(tallyCopies(choose, t1) + "\n----")
      println(tallyCopies(choose, t2))
      
      println("Done")
    }
    
    //higher order
    def testInsert() {
      
    }
    
    def testRank() {//TODO the big one
      
    }
  }
  
  object ArrayTester {
    import j.lang.datatypes.JTypeMacros._
    
    import j.lang.datatypes.array.JArray
    import j.lang.datatypes.array.JArrayType
    import j.lang.datatypes.array.JArrayFrame
    import j.lang.datatypes.array.ArrayImplicits._
    
    import j.lang.datatypes.array.types.JNumberTypes._
    
    import j.lang.primitives.JVerbs._
    
    def testArraysPrint() {//TODO autospacing
      println("\n--Testing Array printing")
      val jarnum1 = JArray[JInt,Int](jINT, List(2, 3, 2), Vector.tabulate(12)((x: Int) => x) )
      val jarnum2 = JArray[JFloat, Double](jFL, List(12), Vector.tabulate(12)((x: Int) => x - 0.5))
      println(jarnum1)
      println(jarnum2)
      println("Done")
    }
  
    def testNumAndSizes() {
      println("\n--Testing Num and Sizes")
      val j1 = integersIndex.monad(JArray.auto[JInt, Int](2,3,2))
      val j2 = integersIndex.monad(JArray.auto[JInt, Int](4,6,5))
      
      println("j1: " + j1.rankItems)
      println("    " + j1.rankSizes)
      println("j2: " + j2.rankItems)
      println("    " + j2.rankSizes)
      
      println("Done")
    }
    
    def testArrayFrames[T <: JArrayType : Manifest](jar: JArray[T]) {
      println("\n--Test Array Frames")
      val jarnum = JArray[JInt,Int](jINT, List(2,3,2), Vector.tabulate(12)((x: Int) => x))
      val jarfrm_ = JArrayFrame(JInfinity, jarnum)
      val jarfrm3 = JArrayFrame(3, jarnum)
      val jarfrm2 = JArrayFrame(2, jarnum)
      val jarfrm1 = JArrayFrame(1, jarnum)
      val jarfrm0 = JArrayFrame(0, jarnum)
      
      println(jarfrm_)
      println(jarfrm3)
      println(jarfrm2)
      println(jarfrm1)
      println(jarfrm0)
      
      val jarfrms = (0 to jar.rank).map((x: Int) => JArrayFrame(x, jar) )
      jarfrms.foreach(println)
      println("--Done")
    }
    
    def testAFShapeTo() {
      println("\n--Testing ShapeTo function")
      val jarf = JArrayFrame(List[JNumber](0,1,1), integersIndex.monad(JArray.auto[JInt, Int](2,2) ))
      println("jarf: " + jarf)
      println(jarf.shapeToNewFrame(List(List(2), List(3), List(2), List())) )
      
      val vec2 = JArrayFrame(List[JNumber](0), JArray.auto[JInt,Int](0,1))
      println("vec2framed: " + vec2)
      val extVec2 = vec2.shapeToNewFrame(List(List(2,3), List()))
      println("extVec2:\n" + extVec2)
      println("Done")
    }
  }

  object LexerTests {
    import j.lang._
    import j.lang.JLexer._
    import java.io.File
    import scala.io.Source

    def testLexerfromFile(ijs: File) {
      Source.fromFile(ijs).getLines.map(sequentialMachine).foreach(
        (l: List[JLexeme]) => println(l mkString ("\n")))
    }

    def testLexer {
      var input = "sum =:+/_6.95*i.3 4 2"
      var erin = "\'"
      val classes = input.map(CharWClass.charClassify)
      classes.foreach(println)

      println()

      smLookUpTable.foreach(x => {
        println("[" + x.mkString(",") + "]")
      })
      println()

      println(sequentialMachine(input).mkString("\n"))

      testLexerfromFile(new File("/home/christopher/j701-user/temp/game_of_life.ijs"))

      //	  println(tokenize(input).mkString("\n"))
      //	  tokenize(erin)	  
    }
  }
}