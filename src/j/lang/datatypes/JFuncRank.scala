package j.lang.datatypes

import j.lang.datatypes.array.types.JNumberTypes._

class JFuncRank(val r1: JNumber, val r2: JNumber, val r3: JNumber)

object JFuncRank {
	def apply[N <% JNumber](r1: N, r2: N, r3: N) = new JFuncRank(r1,r2,r3)
	def apply[N <% JNumber](r2: N, r3: N) 		 = new JFuncRank(r3,r2,r3) 
	def apply[N <% JNumber](r: N) 				 = new JFuncRank(r, r, r)
}

