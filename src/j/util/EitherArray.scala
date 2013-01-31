package j.util

class EitherArray[T,U](val either: Either[Array[T], Array[U]]) {
	lazy val length = either match {
	  case Right(t) => t.length
	  case Left(u)  => u.length
	}
	
	def apply(i: Int)
}

object EitherArray {
}