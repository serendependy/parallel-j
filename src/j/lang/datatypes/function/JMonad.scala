package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._

trait JMonad {
	def leftInDomain(y: JType): Boolean
}