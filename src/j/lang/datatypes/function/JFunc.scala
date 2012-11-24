package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._
import j.util.CMacroType._

abstract class JFunc(rep: String) {

}

object JFunc {
  def inDomain(dom: JType)(argType: JType) = argType isA dom
}