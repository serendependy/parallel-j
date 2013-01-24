package j.lang.datatypes.array

import j.lang.datatypes.array.types._
import j.lang.datatypes.array.types.JNumberTypes._

object ArrayImplicits {
    //Organized by parameter types
  
    //takes Int (or collections thereof
	implicit def intarray(ar: Array[Int]):Array[JInt] =
	  ar.map(new JInt(_))
	implicit def i2j(i: Int): JInt = new JInt(i)

	implicit def d2j(d: Double): JFloat = new JFloat(d)
	implicit def doubarray(ar: Array[Double]):Array[JFloat] = 
	  ar.map(new JFloat(_))
	
	implicit def c2j(c: Char): JChar = new JChar(c)
	implicit def chararray(ar: Array[Char]): Array[JChar] = 
	  ar.map(new JChar(_))
}