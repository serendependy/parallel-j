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
	  def apply(s: Int, c: Int) = new SMFuncRes(JState(s),SMFuncCode(c))
	}
	
	class SMFuncRes(val state: JState.State, val code: FuncCode) {
	  override def toString = "(" + state + "," + code + ")"
	}

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

	private object SMRunningState {
	  def apply(i: Int, j: Int, state: State) = {
		  new SMRunningState(i,j,state, List())
	  }
	}
	
	private class SMRunningState private(private var ip: Int, private var jp: Int,
	    private var sp: State, private var ap: List[JLexeme]){
	  
	  def i = ip
	  def j = jp
	  def accum = ap
	  def state = sp
	  
	  def next(fr: SMFuncRes, line:Seq[CharWClass]) = {
		  val (newJ, newAccum) = fr.code match {
	  		case Pass => (j,accum)
	  		case NextWord => (i,accum)
	  		case EmitWord => (i,
	  		    JLexeme(line.slice(j, i).map(_ char)) +: accum)
	  		case EmitWErr => (-1,
	  		    JLexeme(line.slice(j, i).map(_ char)) +: accum)
	  		case EmitVect => (i,
	  		    JLexeme(line.slice(j, i).map(_ char)) +: accum) //TODO fix
	  		case EmitVErr => (-1,
	  		    JLexeme(line.slice(j, i).map(_ char)) +: accum)//TODO fix
	  		case Stop     => (line.length, accum)
		 }
		  
  	    println(i + "\t" + state + "\t" + line(i) + "\t" + fr)
  	    
  	    ip += 1
  	    jp = newJ
  	    sp = fr.state
  	    ap = newAccum
  	    

	  }
	}
	
	def sequentialMachine2(line: String): List[JLexeme] =
	  sequentialMachine2(SMRunningState(0,-1,JState.Space),line.map(CharWClass(_)))
	  //sequentialMachine2(0,-1, JState.Space,line.map(CharWClass(_)),List())
	
	@tailrec def sequentialMachine2(runState: SMRunningState,
	    line:Seq[CharWClass]): List[JLexeme] = {
	  import runState._
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
	    runState.next(funcRes, line)
	    sequentialMachine2(runState,line)
	  }
	  
	}
	  
//	@tailrec def sequentialMachine2(i:Int,j:Int,
//	    state: State, line: Seq[CharWClass],
//	    accum: List[JLexeme]): List[JLexeme] = {
//	  if (line isEmpty)
//	    accum
//	  else if (i >= line.length) {
//	    (state match {
//	      case JState.Quote => throw new Exception("Open quote!")
//	      case JState.Space => accum
//	      case _ => JLexeme(line.drop(j).map(_ char)) +: accum
//	    }).reverse	    
//	  }
//	  else {
//	    val funcRes = smLookUpTable2(state.id)(line(i).charclass.id)
//	    val newState = funcRes.state
//	    
////	    val (newJ, newAccum) = funcRes.code match {
////	      case Pass => (j,accum)
////	      case NextWord => (i,accum)
////	      case EmitWord => (i,JLexeme(line.slice(j, i).map(_ char)) +: accum)
////	      case EmitWErr => (-1,JLexeme(line.slice(j, i).map(_ char)) +: accum)
////	      case EmitVect => (i,JLexeme(line.slice(j, i).map(_ char)) +: accum) //TODO fix
////	      case EmitVErr => (-1,JLexeme(line.slice(j, i).map(_ char)) +: accum)//TODO fix
////	      case Stop   	=> (line.length, accum)
////	    }
////	    println(i + "\t" + state + "\t" + line(i) + "\t" + funcRes)	    
////	    sequentialMachine2(i+1,newJ,newState,line,newAccum)
//	  }
//	}
}