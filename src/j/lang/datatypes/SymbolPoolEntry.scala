package j.lang.datatypes

import j.lang.datatypes.JArray._

//TODO better implementation of field name?
class SymbolPoolEntry(val name: JArray[String], 
    val value: JArray[JArrayType], val flag: Int, val sn: Int, 
    val next: Int, val prev: Int) {

}

/* symbol pool entry                           LINFO entry                 */
/* name - name on LHS of assignment         or locale name                 */
/* val  - value                             or locale search path          */
/* flag - various flags                                                    */
/* sn   - script index                                                     */
/* next - index of successor   in hash list or 0                           */
/* prev - index of predecessor in hash list or address of hash entry       */