package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._

trait JDyad extends JMonad {
	def rightInDomain(x: JType): Boolean
	
	def bothInDomain(x: JType, y: JType) = rightInDomain(y) && leftInDomain(x)
}