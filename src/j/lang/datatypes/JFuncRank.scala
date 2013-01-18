package j.lang.datatypes

import j.lang.datatypes.array.types.JNumber


object JFuncRank {
	def apply[N <% JNumber](r1: N, r2: N, r3: N) = new JFuncRank(r1,r2,r3)
}

class JFuncRank(r1: JNumber, r2: JNumber, r3: JNumber)