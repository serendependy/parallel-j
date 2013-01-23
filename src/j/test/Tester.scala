package j.test

object Tester {
  
  def main(args: Array[String]) {
    {
     import ArrayTester._
     testArrays()
    }
	 
    
  }
  
  object ArrayTester {
    import j.lang.datatypes.JTypeMacros._
    import j.lang.datatypes.array.JArray
    import j.lang.datatypes.array.JArrayType
    import j.lang.datatypes.array.ArrayImplicits._
    
    def testArrays() {
      val jarnum1 = JArray(jINT, List(2, 3, 2), Array.tabulate(12)((x: Int) => x) )
      val jarnum2 = JArray(jFL, List(12), Array.tabulate(12)((x: Int) => x - 0.5))
      println(jarnum1)
      println(jarnum2)
      
      
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