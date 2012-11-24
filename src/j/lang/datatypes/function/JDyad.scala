package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._

class JDyad(rep: String, ldomType: JType, rdomType: JType) extends
	JFunc(rep, if ( (ldomType | rdomType) isA jVERB) jCONJ else jVERB){
	
}