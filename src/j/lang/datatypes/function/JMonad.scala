package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._

class JMonad(rep: String, ldomType: JType) extends JFunc(rep, 
    if (ldomType isA jNOUN) jVERB else jADV) {

}