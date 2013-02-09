package j.test

object Tester {
  
  def main(args: Array[String]) {
    {
     import ArrayTester._
     import j.lang.datatypes.array.JArray
     import j.lang.datatypes.array.ArrayImplicits._
     
     import j.lang.datatypes.JTypeMacros._
     
     testArraysPrint()
     val jarnum = JArray(jINT, List(5,5), Array.tabulate(5,5)((x: Int, y: Int) => if (x == y) 1 else 0).flatten)
     testArrayFrames(jarnum)
    }
    
    {
      import FunctionTester._
      
      testIntegers()
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
      println("\nTesting integers")
      val r3s232 = integersIndex.monad(JArray.auto(2,3,2))
      println(r3s232.shape + "\n" + (r3s232.ravel.map(_ v).mkString(" ")))
      println(r3s232)
      println("Done")
    }
    
    def testNegate() {
      println("\nTesting Negate")
      val ar: JArray[JNumber] = integersIndex.monad(JArray.auto(2,3,2))
      val res = negateMinus.monad(ar)
      println("Done")
    }
  }
  
  object ArrayTester {
    import j.lang.datatypes.JTypeMacros._
    
    import j.lang.datatypes.array.JArray
    import j.lang.datatypes.array.JArrayType
    import j.lang.datatypes.array.JArrayFrame
    import j.lang.datatypes.array.ArrayImplicits._
    
    import j.lang.datatypes.array.types.JNumberTypes._
    
    def testArraysPrint() {
      val jarnum1 = JArray(jINT, List(2, 3, 2), Array.tabulate(12)((x: Int) => x) )
      val jarnum2 = JArray(jFL, List(12), Array.tabulate(12)((x: Int) => x - 0.5))
      println(jarnum1)
      println(jarnum2)
      
      
    }
    
    def testArrayFrames[T <% JArrayType : Manifest](jar: JArray[T]) {
      val jarnum = JArray(jINT, List(2,3,2), Array.tabulate(12)((x: Int) => x))
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