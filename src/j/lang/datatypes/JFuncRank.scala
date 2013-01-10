package j.lang.datatypes

object JFuncRank {
	def apply(r1: Int, r2: Int, r3: Int) = new JFuncRank(r1,r2,r3)
	def apply(r: Int) = new JFuncRank(r, r, r)
}

class JFuncRank(r1: Int, r2: Int, r3: Int) //TODO extend to type which includes infinity