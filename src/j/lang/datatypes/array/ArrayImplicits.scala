package j.lang.datatypes.array

import j.lang.datatypes.array.types._
import j.lang.datatypes.array.types.JNumberTypes._

import j.lang.datatypes.JTypeMacros._

object ArrayImplicits {
    //Organized by parameter types
  
    //takes Int (or collections thereof
	implicit def intarray(ar: Vector[Int]):Vector[JInt] =
	  ar.map(new JInt(_))
	implicit def jintlist(ar: List[JInt]): List[Int] =
	  ar.map(_.v)
	implicit def i2j(i: Int): JInt = new JInt(i)
	implicit def j2i(ji: JInt): Int = ji.v

	implicit def d2j(d: Double): JFloat = new JFloat(d)
	implicit def doubarray(ar: Vector[Double]):Vector[JFloat] = 
	  ar.map(new JFloat(_))
	
	implicit def c2j(c: Char): JChar = new JChar(c)
	implicit def chararray(ar: Vector[Char]): Vector[JChar] = 
	  ar.map(new JChar(_))
	  
	implicit def b2j(b: Boolean): JInt = new JInt(if (b) 1 else 0)
	
	//TODO salvage my soul
	/* This problem occurred because I wanted to treat JArray[JInt] like JArray[JNumber]
	 * but the type parameter of JArray isn't covariant. I have yet to figure out a 
	 * good way to solve this.
	 * 
	 * One option might be to implement the actual primitives as objects which take type arguments,
	 * and then let all user constructed primitives be of a different type.
	 */
//	implicit def jia2jna(jia: JArray[JInt]): JArray[JNumber] = 
//	  JArray(jNUMERIC, jia.shape, jia.ravel.map((x: JInt) => x))
}