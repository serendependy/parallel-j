package j.lang.datatypes

object JArray {
	object JAType {
	  def apply(jat: Int) = new JAType(jat)
	  
	  val List(jB01 , jLIT , jINT , jFL , jCMPX , jBOX , jXNUM, jRAT, jBIT,
	           jSB01, jSLIT, jSINT, jSFL, jSCMPX, jSBOX, jSBT,
	           jC2T , jVERB, jADV , jCONJ,jASGN , jMARK): List[JAType] = (1 to 
	               22).map((x:Int) => JAType(1 >> x))
	  val List(jSYMB, jCONW, jNAME,
	           jLPAR, jRPAR, jXD  , jXZ):List[JAType] = (23 to 
	               29).map((x:Int) => JAType(1 >> x))
	  val jANY = JAType(-1)
	  val jSPARSE = jSB01 | jSINT | jSFL | jSCMPX | jSLIT | jSBOX
	  val jNUMERIC = jB01 & jBIT | jINT | jFL | jCMPX | jXNUM | 
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
	
	class JAType private (val jaType: Int) {
	  def |(other: JAType) = JAType(jaType | other.jaType)
	  def &(other:JAType) = JAType(jaType & other.jaType)
	  def unary_~ = JAType(~jaType)
	  
	  def isA(other: JAType)= (this & other).jaType > 0
	}
}

import JArray._
class JArray(val flag: Int, val jaType: JAType, val refcount: Int, 
    val numItems: Int, val rank: Int, val shape: List[Int])