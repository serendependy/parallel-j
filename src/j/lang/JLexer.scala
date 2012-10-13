package j.lang

import scala.annotation.tailrec

object JLexer {

  case class JLexeme(chars: String)
  
  	object JState extends Enumeration {
	  type State = Value
	  val Space 		= Value(0)
	  val Other 		= Value(1)
	  val AlphNum 		= Value(2)
	  val N 			= Value(3)
	  val NB			= Value(4)
	  val `NB.` 		= Value(5)
	  val Numeric 		= Value(6)
	  val Quote 		= Value(7)
	  val EvenQuotes 	= Value(8)
	  val Comment 		= Value(9)
	}
	
	object JCharClass extends Enumeration {
	  type CharClass = Value
	  val Other 		= Value(0)
	  val Space 		= Value(1)
	  val AlphNotNB 	= Value(2)
	  val N				= Value(3)
	  val B 			= Value(4)
	  val Numeric 		= Value(5)
	  val Period		= Value(6)
	  val Colon			= Value(7)
	  val Quote			= Value(8)
	}

	import JState._
		
	sealed abstract class SMFuncCode
	case object NextLex extends SMFuncCode
	case object EmitLex extends SMFuncCode
	case object Pass	extends SMFuncCode
	
	object SMFuncRes {
	  def apply(str: String) = {
	    val excep = new Exception("SMFuncRes: Bad domain: " + str)

	    if (str.isEmpty || str.length > 2) {
	      throw excep
	    }
	    else {
	      val myState = str.charAt(0) match {
	        case 'S' => Space
	        case 'X' => Other
	        case 'A' => AlphNum
	        case 'N' => N
	        case 'B' => NB
	        case 'C' => `NB.`
	        case '9' => Numeric
	        case 'Q' => Quote
	        case 'E' => EvenQuotes
	        case 'Z' => Comment
	        case _ => throw excep
	      }

	      val myCode = if (str.length == 1) Pass else str.charAt(1) match {
	        case 'I' => EmitLex
	        case 'N' => NextLex
	        case _ => throw excep
	      }
	      
	      new SMFuncRes(myState, myCode)
	    }
	  }
	}
	
	class SMFuncRes(val state: JState.State, val code: SMFuncCode) {
	  override def toString = "(" + state + "," + code + ")"
	}
	
	
	val smLookupTable = """
XN  S   AN  NN  AN  9N  XN  XN  QN
XI  SI  AI  NI  AI  9I  X   X   QI
XI  SI  A   A   A   A   X   X   QI
XI  SI  A   A   B   A   X   X   QI
XI  SI  A   A   A   A   C   X   QI
Z   Z   Z   Z   Z   Z   X   X   Z
XI  SI  9   9   9   9   9   X   QI
Q   Q   Q   Q   Q   Q   Q   Q   E
XI  SI  AI  NI  AI  9I  XI  XI  Q
Z   Z   Z   Z   Z   Z   Z   Z   Z
""".split("\n").drop(1).map(_.split(" +").map(SMFuncRes(_))
)
	    
	import JCharClass._
	    
	private case class CharWClass(val char: Char, val charclass: CharClass)
  
	private val charClasses = (0 until 256).map((i:Int) => 
	  initCharClassify(i.toChar)).toArray

	private def initCharClassify(c: Char)= {
	  import JCharClass._
	  if 		(c == ' ')		 Space
	  else if 	(c.isDigit 
			  		|| c == '_') Numeric
	  else if 	(c == 'N') 		 N
	  else if 	(c == 'B') 	 	 B
	  else if 	(c.isLetter) 	 AlphNotNB
	  else if 	(c == '.') 		 Period
	  else if 	(c == ':') 		 Colon
	  else if 	(c == '\'') 	 Quote
	  else 						 Other
	}
	
	def charClassify(c: Char) = charClasses(c.toInt)
	
	def tokenize(line: String):List[JLexeme] = {
	  tokenize(line.map((x: Char) => CharWClass(x,charClassify(x)) ) )
	}
	
	def tokenize(line: Seq[CharWClass]):List[JLexeme] = {
	  sequentialMachine(0,JState.Space,line,List())
	}
	
	@tailrec def sequentialMachine(i:Int, 
	    state: State, line: Seq[CharWClass], 
	    accum: List[JLexeme]): List[JLexeme] = {
	  if (line isEmpty) {
	    accum
	  }
	  else if (i == line.length) {
//	    if (state == JState.Quote) {
//	      throw new Exception("Open quote!")
//	    }
//	    if ()
	    (state match {
	      case JState.Quote => throw new Exception("Open quote!")
	      case JState.Space => accum
	      case _ => JLexeme(line.map(_ char).mkString) +: accum
	    }).reverse
	  }
	  else {
	    val funcRes = smLookupTable(state.id)(line(i).charclass.id)
	    
	    val (newi, newAccum, newLine):(Int,List[JLexeme],Seq[CharWClass]) = funcRes.code match {
	      case Pass => (i,accum,line)
	      case NextLex => (0, accum, line.drop(i) )
	      case EmitLex => (0, JLexeme(line.slice(0,i).map(_ char).mkString) +: accum,
	          line.drop(i))
	    }
	    
	    println(i + "\t" + state + "\t" + line(i) + "\t" + funcRes)
	    
	    sequentialMachine(newi+1, funcRes.state, newLine, newAccum)
	  }
	}
	
}