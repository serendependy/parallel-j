package j.lang.datatypes

object JArray {
	object JAType {
	  def apply(jat: Int) = new JAType(jat)
	  
	  val List(jB01 , jLIT , jINT , jFL , jCMPX , jBOX , jXNUM, jRAT, jBIT,
	           jSB01, jSLIT, jSINT, jSFL, jSCMPX, jSBOX, jSBT,
	           jC2T , jVERB, jADV , jCONJ,jASGN , jMARK) = (1 to 22).map(1 << _)
	  val List(jSYMB, jCONW, jNAME,
	           jLPAR, jRPAR, jXD  , jXZ) = (23 to 29).map(1 << _)
	}
	
	class JAType private (val jaType: Int) {
	  def or(other: JAType) = JAType(jaType | other.jaType)
	  def and(other:JAType) = JAType(jaType & other.jaType)
	  
	  def isA(other: JAType)= (this and other).jaType > 0
	}
}

import JArray._
class JArray(val flag: Int, val jaType: JAType, val refcount: Int, 
    val numItems: Int, val rank: Int, val shape: List[Int])