package j.util

class CMacroType[A] (val bitmask: Int) {
	def &(other: CMacroType[A]) = new CMacroType[A](bitmask & other.bitmask)
	def |(other: CMacroType[A]) = new CMacroType[A](bitmask | other.bitmask)
	def unary_~ = new CMacroType[A](~bitmask)
}