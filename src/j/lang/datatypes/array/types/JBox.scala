package j.lang.datatypes.array.types

import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.array.JArray

import j.lang.datatypes.JTypeMacros._

case class JBox(val unboxed: JArray[_]) extends JArrayType(jBOX)