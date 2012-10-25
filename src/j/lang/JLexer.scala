package j.lang

import scala.annotation.tailrec

object JLexer {

	class JLexeme(val chars: String) {
	  def +:(str: String) = JLexeme(this.chars + str)
	  override def toString = "JLexeme("+chars+")"
	}
	object JLexeme {
	  def apply(chars: String) = new JLexeme(chars)
	  def apply(chars: Seq[Char]) = new JLexeme(chars.mkString(""))
	}
  
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
		
	object SMFuncCode extends Enumeration {
	  type FuncCode = Value
	  val Pass 		= Value(0)
	  val NextWord	= Value(1)
	  val EmitWord 	= Value(2)
	  val EmitWErr	= Value(3)
	  val EmitVect  = Value(4)
	  val EmitVErr	= Value(5)
	  val Stop		= Value(6)
	}

	import SMFuncCode._
	object SMFuncRes { //TODO needs to be done to support ;: given a table
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
	        case 'I' => EmitWord
	        case 'N' => NextWord
	        case _ => throw excep
	      }
	      
	      new SMFuncRes(myState, myCode)
	    }
	  }
	  
	  def apply(s: Int, c: Int) = new SMFuncRes(JState(s),SMFuncCode(c))
	}
	
	class SMFuncRes(val state: JState.State, val code: FuncCode) {
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

	val smLookUpTable2 = """
' X    S    A    N    B    9    D    C    Q ']0
 1 1  0 0  2 1  3 1  2 1  6 1  1 1  1 1  7 1  NB. 0 space
 1 2  0 3  2 2  3 2  2 2  6 2  1 0  1 0  7 2  NB. 1 other
 1 2  0 3  2 0  2 0  2 0  2 0  1 0  1 0  7 2  NB. 2 alp/num
 1 2  0 3  2 0  2 0  4 0  2 0  1 0  1 0  7 2  NB. 3 N
 1 2  0 3  2 0  2 0  2 0  2 0  5 0  1 0  7 2  NB. 4 NB
 9 0  9 0  9 0  9 0  9 0  9 0  1 0  1 0  9 0  NB. 5 NB.
 1 4  0 5  6 0  6 0  6 0  6 0  6 0  1 0  7 4  NB. 6 num
 7 0  7 0  7 0  7 0  7 0  7 0  7 0  7 0  8 0  NB. 7 '
 1 2  0 3  2 2  3 2  2 2  6 2  1 2  1 2  7 0  NB. 8 ''
 9 0  9 0  9 0  9 0  9 0  9 0  9 0  9 0  9 0  NB. 9 comment
""".split("\n").drop(2).map(_.drop(1).split("  ").dropRight(1).map(entry =>{
  val Array(s,c) = entry.split(" ").map(_ toInt)
  SMFuncRes(s,c)
}))
	    
	import JCharClass._
	
	//TODO this also needs to be done in a way that extends over arbitrary classifications
	class CharWClass(val char: Char, val charclass: CharClass) {
	  override def toString() = "(" + char + "," + charclass + ")"
	}
	object CharWClass {
		def apply(char: Char, charClass: CharClass) = new CharWClass(char,charClass)
	  
		def apply(char: Char) = new CharWClass(char, charClassify(char))
		
		val charClasses = (0 until 256).map((i:Int) => 
			initCharClassify(i.toChar)).toArray

		def initCharClassify(c: Char)= {
			import JCharClass._
			if 		(c == ' ')		 Space
			else if 	(c.isDigit 
				|| c == '_') 		Numeric
			else if 	(c == 'N') 	N
			else if 	(c == 'B') 	B
			else if 	(c.isLetter)AlphNotNB
			else if 	(c == '.') 	Period
			else if 	(c == ':') 	Colon
			else if 	(c == '\'') Quote
			else 					Other
		}
		
		def charClassify(c: Char) = charClasses(c.toInt)
	}
	
	def tokenize(line: String):List[JLexeme] = {
	  tokenize(line.map(CharWClass(_) ) )
	}
	
	def tokenize(line: Seq[CharWClass]):List[JLexeme] = {
	  sequentialMachine(0,JState.Space,line,List())
	}
	
	def sequentialMachine2(line: String): List[JLexeme] = 
	  sequentialMachine2(0,-1, JState.Space,line.map(CharWClass(_)),List())
	
	@tailrec def sequentialMachine2(i:Int,j:Int,
	    state: State, line: Seq[CharWClass],
	    accum: List[JLexeme]): List[JLexeme] = {
	  if (line isEmpty)
	    accum
	  else if (i >= line.length) {
	    (state match {
	      case JState.Quote => throw new Exception("Open quote!")
	      case JState.Space => accum
	      case _ => JLexeme(line.drop(j).map(_ char)) +: accum
	    }).reverse	    
	  }
	  else {
	    val funcRes = smLookUpTable2(state.id)(line(i).charclass.id)
	    val newState = funcRes.state
	    
	    val (newJ, newAccum) = funcRes.code match {
	      case Pass => (j,accum)
	      case NextWord => (i,accum)
	      case EmitWord => (i,JLexeme(line.slice(j, i).map(_ char)) +: accum)
	      case EmitWErr => (-1,JLexeme(line.slice(j, i).map(_ char)) +: accum)
	      case EmitVect => (i,JLexeme(line.slice(j, i).map(_ char)) +: accum) //TODO fix
	      case EmitVErr => (-1,JLexeme(line.slice(j, i).map(_ char)) +: accum)//TODO fix
	      case Stop   	=> (line.length, accum)
	    }
	    println(i + "\t" + state + "\t" + line(i) + "\t" + funcRes)	    
	    sequentialMachine2(i+1,newJ,newState,line,newAccum)
	  }
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
	      case NextWord => (0, accum, line.drop(i) )
	      case EmitWord => (0, JLexeme(line.slice(0,i).map(_ char).mkString) +: accum,
	          line.drop(i))
	    }
	    
	    println(i + "\t" + state + "\t" + line(i) + "\t" + funcRes)
	    
	    sequentialMachine(newi+1, funcRes.state, newLine, newAccum)
	  }
	}
	
}