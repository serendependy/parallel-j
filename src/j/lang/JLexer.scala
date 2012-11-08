package j.lang

import scala.annotation.tailrec

object JLexer {

	class JLexeme(val chars: String, val state: JState.State) {
	  def +:(str: String) = JLexeme(this.chars + str, state)
	  override def toString = "JLexeme("+chars+","+state+")"
	}
	object JLexeme {
	  def apply(chars: String, state: JState.State) = new JLexeme(chars, state)
	  def apply(chars: Seq[Char], state: JState.State) = new JLexeme(chars.mkString(""), state)
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
			if 		(c == ' ')		Space
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
	  case class EmitState(r: State, k: Int)
	  
	  def apply(i: Int, j: Int, state: State) = {
		  new SMRunningState(i,j,state, List(), None)
	  }
	}
	
	private class SMRunningState private(private var ip: Int, private var jp: Int,
	    private var sp: State, private var ap: List[JLexeme],
	    private var vp: Option[SMRunningState.EmitState]) {
	  
	  def i = ip
	  def j = jp
	  def accum = ap
	  def state = sp
	  def evState = vp

	  private def ev(line:Seq[CharWClass]) = {
	    import SMRunningState._
		    evState match {
		      case None => JLexeme(line.slice(j, i).map(_ char), state) +: accum
		      case v: Some[EmitState] => {
		        if (v.get.r == state) {
		          accum.head.+:(line.slice(v.get.k,i).map(_ 
		          char).mkString) +: accum.drop(1)
		        }
		        else {
		          JLexeme(line.slice(j, i).map(_ char),state) +: accum
		        }
		      }
		    }	    
	  }
	  
	  def ew(line:Seq[CharWClass]) = JLexeme(line.slice(j, i).map(_ char),state) +: accum
	  
	  import SMRunningState._
	  def next(fr: SMFuncRes, line:Seq[CharWClass]) = {

		ap = fr.code match {
		  case Pass     => accum
		  case NextWord => accum
		  
		  case EmitWord => ew(line) 
		  case EmitWErr => ew(line)
		  
		  case EmitVect => ev(line)
		  case EmitVErr => ev(line)
		}
		vp = fr.code match {
		  case Pass     => evState
		  case NextWord => evState
		  case EmitWord => None
		  case EmitWErr => None
		  case _ => Some(EmitState(state,i))
		}
		
		println(Array(i,j,state,line(i),fr).map(_ toString).mkString("\t"))
		
		sp = fr.state		
		jp = fr.code match {
		  case EmitWErr => -1
		  case EmitVErr => -1
		  case Pass     => j
		  case _        => i
		}
		ip += 1
	  }
	  
	  private val endState = smLookUpTable2(JCharClass.Space.id)
	  def finalize(fr: SMFuncRes, line:Seq[CharWClass]) = {
		  ap = ev(line)
	  }
//	    (state match {
//	      case JState.Quote => throw new Exception("Open quote!")
//	      case JState.Space => accum
//	      case _ => {
//	        
//	      }
//	    }).reverse
//	  }
	}
	
	def sequentialMachine2(line: String): List[JLexeme] =
	  sequentialMachine2(SMRunningState(0,-1,JState.Space),line.map(CharWClass(_)))
	
	@tailrec def sequentialMachine2(runState: SMRunningState,
	    line:Seq[CharWClass]): List[JLexeme] = {
	  import runState._
	  if (line isEmpty)
	    accum.reverse
	  else if (i >= line.length) {
	    val funcRes = smLookUpTable2(state.id)(JCharClass.Space.id)
	    runState.finalize(funcRes,line)
	    accum.reverse
	  }
	  else {
	    val funcRes = smLookUpTable2(state.id)(line(i).charclass.id)
	    runState.next(funcRes, line)
	    sequentialMachine2(runState,line)
	  }
	}
}