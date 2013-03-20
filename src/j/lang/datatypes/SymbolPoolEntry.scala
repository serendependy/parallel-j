package j.lang.datatypes

import j.lang.datatypes.array._

//TODO better implementation of field name?
class SymbolPoolEntry[T <: JArrayType](val name: String, 
    val value: JArray[T], val flag: Int, val sn: Int, 
    val next: Int, val prev: Int) {

}

/* symbol pool entry                           LINFO entry                 */
/* name - name on LHS of assignment         or locale name                 */
/* val  - value                             or locale search path          */
/* flag - various flags                                                    */
/* sn   - script index                                                     */
/* next - index of successor   in hash list or 0                           */
/* prev - index of predecessor in hash list or address of hash entry       */