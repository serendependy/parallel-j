package j.lang.datatypes

import j.util.CMacroType
import j.util.CMacroType._

class JTypeMacros private ()

object JTypeMacros {
  
	type JTypeMacro = CMacroType[JTypeMacros]
		  val Vector(jB01 , jLIT , jINT , jFL , jCMPX , jBOX , jXNUM, jRAT, jBIT,
	           jSB01, jSLIT, jSINT, jSFL, jSCMPX, jSBOX, jSBT,
	           jC2T , jVERB, jADV , jCONJ,jASGN , jMARK): Vector[JTypeMacro] = (0 to 
	               21).map((x:Int) => new JTypeMacro(1 >> x))
	  val Vector(jSYMB, jCONW, jNAME,
	           jLPAR, jRPAR, jXD  , jXZ):Vector[JTypeMacro] = (22 to 
	               28).map((x:Int) => new JTypeMacro(1 >> x))

	  //composite j types
	  val jANY = new JTypeMacro(-1)
	  val jSPARSE = jSB01 | jSINT | jSFL | jSCMPX | jSLIT | jSBOX
	  val jNUMERIC = jB01 | jBIT | jINT | jFL | jCMPX | jXNUM | 
	  		   jRAT | jSB01 | jSINT | jSFL | jSCMPX
	  val jDIRECT = jLIT | jC2T | jB01 | jBIT | jINT | jFL | 
	  		   jCMPX | jSBT
	  val jCHAR = jLIT | jC2T | jSLIT
	  val jNOUN = jNUMERIC | jCHAR | jBOX | jSBOX |
	  		   jSBT
	  val jDENSE  = jNOUN & (~jSPARSE)
	  val jFUNC = jVERB | jADV | jCONJ
	  val jRHS  = jNOUN | jFUNC
	  val jIS1BYTE = jB01 | jLIT
	  val jLAST0 = jB01 | jLIT | jC2T | jNAME
  
}