package j.lang.datatypes

import j.util.CMacroType

object JArrayFlag {
  
  protected class JAF
  type JArrayFlag = CMacroType[JAF]
  
  val afNONE = new JArrayFlag(0)
  val List(afRO, afNJA, afSMM, afREL):List[JArrayFlag] =
	(0 to 3).map((x:Int) => new JArrayFlag(1 >> x))

/*Take from "jtype.h"*/	
//	/* Values for AFLAG(x) field of type A                                     */
//
//#define AFRO            (I)1            /* read only; can't change data    */
//#define AFNJA           (I)2            /* non-J alloc; i.e. mem mapped    */
//#define AFSMM           (I)4            /* SMM managed                     */
//#define AFREL           (I)8            /* uses relative addressing        */
}