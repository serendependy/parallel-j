package j.util

class CMacroType[A] (val bitmask: Int) {
	def &(other: CMacroType[A]) = new CMacroType[A](bitmask & other.bitmask)
	def |(other: CMacroType[A]) = new CMacroType[A](bitmask | other.bitmask)
	def unary_~ = new CMacroType[A](~bitmask)
	def ==(other: CMacroType[A]) = bitmask == other.bitmask
	def !=(other: CMacroType[A]) = bitmask != other.bitmask
	def isA(other: CMacroType[A])= (this & other) != CMacroType.NullMacroType
}

object CMacroType {
  val NullMacroType = new CMacroType[Nothing](0)
}