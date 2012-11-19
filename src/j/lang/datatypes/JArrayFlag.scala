package j.lang.datatypes

import j.util.CMacroType

object JArrayFlag {
  
  protected class JAF
  type JArrayFlag = CMacroType[JAF]
  
  val List(afRO, afNJA, afSMM, afREL):List[JArrayFlag] =
	(0 to 3).map((x:Int) => new JArrayFlag(1 >> x))
}