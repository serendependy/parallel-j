package j.lang.datatypes.array.types

import scala.Array.canBuildFrom

object NumericImplicits {
	implicit def int2jint(i: Int): JInt = new JInt(i)
	implicit def doub2jfloat(d: Double): JFloat = new JFloat(d)
	
	implicit def ai2jai(ai: Array[Int]): Array[JNumber] = ai.map(new JInt(_))
	implicit def ad2jaf(af: Array[Double]): Array[JNumber] = af.map(new JFloat(_)) 
}