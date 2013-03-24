package j.lang.datatypes

import j.lang.datatypes.array.types.JNumberTypes._

class JFuncRank(val r1: JNumber, val r2: JNumber, val r3: JNumber) {
  override def toString = "JFuncRank(" + r1 +"," + r2 +"," + r3 + ")"
}

object JFuncRank {
	def apply[N1 <% JNumber, N2 <% JNumber, N3 <% JNumber](r1: N1, r2: N2, r3: N3) = new JFuncRank(r1,r2,r3)
	def apply[N2 <% JNumber, N3 <% JNumber](r2: N2, r3: N3)= new JFuncRank(r3,r2,r3) 
	def apply[N <% JNumber](r: N) = new JFuncRank(r, r, r)
}

