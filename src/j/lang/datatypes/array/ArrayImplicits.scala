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
}