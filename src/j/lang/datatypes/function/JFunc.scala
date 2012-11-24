package j.lang.datatypes.function

import j.lang.datatypes.JTypeMacros._
import j.util.CMacroType._

abstract class JFunc(val rep: String, val funcType: JType) {

}

object JFunc {
  def inDomain(dom: JType)(argType: JType) = argType isA dom
}