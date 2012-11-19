package j.lang.datatypes

import j.lang.datatypes.SymbolPoolEntry._

class JName(val hash: Int, val entry: SymbolPoolEntry, val sn: Int, val m: Byte, 
    val flag: Byte, val s: String) {
	
}

//typedef struct{UI hash;I sn;L*e;UC m;C flag,s[1];} NM;

/* hash: hash for  non-locale part of name                                 */
/* m:    length of non-locale part of name                                 */
/* sn:   symbol table number on last reference                             */
/* e:    symbol pool entry   on last reference                             */
/* s:    points to string part of full name (1 to ?? characters)           */ 