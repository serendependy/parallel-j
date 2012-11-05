package j.test

import j.lang._

import j.lang.JLexer._

object Tester {
	def main(args:Array[String]) {
	  var input = "sum =:+/_6.95*i.3 4"
	  var erin  = "\'"
	  val classes = input.map(CharWClass.charClassify)
	  classes.foreach(println)
	  
	  println()
	  
	  smLookUpTable2.foreach(x => {
	    println("[" + x.mkString(",") + "]")
	  })
	  println()
	  
	  println(sequentialMachine2(input).mkString("\n"))
	  
//	  println(tokenize(input).mkString("\n"))
//	  tokenize(erin)
	}
}