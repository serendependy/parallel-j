package j.lang.datatypes.array.types

import scala.math.Ordered

import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.JTypeMacros._

object JNumberTypes {
  import JReal._
sealed abstract class JNumber(jtype: JTypeMacro) extends JArrayType(jtype){
	def +(o: JNumber): JNumber
	def -(o: JNumber): JNumber
	def *(o: JNumber): JNumber
	def %(o: JNumber): JNumber
	
	def **(o: JNumber): JNumber
	def %%(o: JNumber): JNumber
	
	def unary_| : JNumber
	def unary_- : JNumber
	
	def |(o : JNumber): JNumber
	
	override def toString: String
}

object JNumber {
  def apply(a: Any) = a match {
    case i:Int => new JInt(i)
    case d:Double=> new JFloat(d)
    case _ => JNaN
  }
}

object JReal {
  sealed abstract class Signum
  final object Neg extends Signum
  final object Neut extends Signum
  final object Pos extends Signum
  
  val NOne = new JInt(-1)
  val Zero = new JInt(0)
  val One  = new JInt(1)
  val Two  = new JInt(2)
  val E    = new JFloat(math.E)
  val Pi   = new JFloat(math.Pi)
}

sealed abstract class JReal(jtype: JTypeMacro) extends JNumber(jtype) with Ordered[JReal] {
  def signum: Signum
}

final object JNaN extends JNumber(jFL){
  
  def compare(r: JReal) = 0
  
  def +(o: JNumber) = this
  def -(o: JNumber) = this
  def *(o: JNumber) = this
  def %(o: JNumber) = this
  
  def **(o: JNumber) = this
  def %%(o: JNumber) = this

  def unary_- = this
  def unary_| = this
  
  def |(o: JNumber) = this
  
  override def toString = "_."
}

sealed abstract class JInfinite extends JReal(jFL) with Ordered[JReal] {
  
  def %(o: JNumber) = o match {
    case fi: Finite => this
    case inf:JInfinite=> JNaN
    case JNaN => JNaN
  }
  
  def %%(o: JNumber) = throw new Exception()//TODO 0s and NaN
  
  def unary_| = JInfinity
}

sealed abstract class Finite(jtype: JTypeMacro) extends JReal(jtype) with Ordered[JReal]{
  def compareFinite(fi: Finite): Int
  
  def compare(r: JReal) = r match {
    case JInfinity => -1
    case JNegativeInfinity => 1
    case fi:Finite => compareFinite(fi)
  }
  
  protected def +~(fi: Finite): JNumber
  protected def -~(fi: Finite): JNumber
  protected def *~(fi: Finite): JNumber
  protected def %~(fi: Finite): JNumber
  protected def **~(fi: Finite): JNumber
  protected def %%~(fi: Finite): JNumber
  
  protected def |~(fi: Finite): JNumber
  
  def +(o: JNumber) = o match {
    case inf: JInfinite => inf + o
    case fi: Finite => this +~ fi
    case JNaN => JNaN
  }
  
  def -(o: JNumber) = o match {
    case inf: JInfinite => (-inf) + this
    case fi: Finite => this -~ fi
    case JNaN => JNaN
  }
  
  def *(o: JNumber) = o match {
    case inf: JInfinite => inf * this
    case fi: Finite => this *~ fi
    case JNaN => JNaN
  }
  
  def %(o: JNumber) = o match {
    case inf: JInfinite => Zero
    case fi: Finite => this %~ fi
    case JNaN => JNaN
  }
  
  def **(o: JNumber) = o match {
    case inf: JInfinite => inf ** this
    case fi: Finite => this **~ fi
    case JNaN => JNaN
  }
  
  def %%(o: JNumber) = o match {
    case JInfinity => JInfinity
    case JNegativeInfinity => throw new Exception()//TODO complex value
    case fi: Finite => this %%~ fi
    case JNaN => JNaN
  }
  
  def |(o: JNumber) = o match {
    case inf: JInfinite => JNaN
    case JNaN => JNaN
    case fi: Finite => this |~ fi
  }
}


final object JInfinity extends JInfinite with Ordered[JReal] {
  import JReal._
  
  def signum = Pos
  
  def compare(r: JReal): Int = r match {
    case JInfinity => 0
    case _ => -1
  }
  
  def +(o: JNumber) = o match {
    case JNegativeInfinity => JNaN //TODO JNaN error
    case JNaN => JNaN
    case r:JReal => this
  }
  
  def -(o: JNumber) = o match {
    case JInfinity => JNaN //TODO JNaN error
    case JNaN => JNaN
    case r:JReal => JNegativeInfinity
  }
  
  def *(o: JNumber) = o match {
    case JNaN => JNaN
    case r:JReal => (r signum) match {
      case Pos => JInfinity
      case Neut=> Zero
      case Neg => JNegativeInfinity
    }
  }

  def **(o: JNumber) = o match {
    case r: JReal => if (r < JReal.Zero) JReal.Zero else if (r == JReal.Zero) JReal.One else this
    case _ => throw new Exception()//TODO should be limit error
  }
  
  def |(o: JNumber) = o
  
  def unary_- = JNegativeInfinity
  
  override def toString = "_"
}

final object JNegativeInfinity extends JInfinite with Ordered[JReal] {
	import JReal._
	
	def signum = Neg
	
	def compare(r: JReal): Int = r match {
	  case JNegativeInfinity => 0
	  case _ => 1
	}
	
	def +(o: JNumber) = o match {
	  case JInfinity => JNaN //TODO JNaN error
	  case JNaN => JNaN
	  case r:JReal => this
	}
	
	def -(o: JNumber) = o match {
	  case JNegativeInfinity => JNaN //TODO JNaN error
	  case JNaN => JNaN
	  case r: JReal => JInfinity
	}
	
	def *(o: JNumber) = o match {
	  case JNaN => JNaN
	  case r:JReal => (r signum) match {
	    case Pos => JNegativeInfinity
	    case Neut=> Zero
	    case Neg => JInfinity
	  }
	}
	
	def **(o: JNumber) = o match {
	  case r: JReal => if (r < JReal.Zero) JReal.Zero else if (r == JReal.Zero) JReal.One else r match {
	    case i: JInt => if ((i | JReal.Two) == JReal.Zero) JInfinity else this
	    case _ => throw new Exception()//TODO implement
	  }
	  case _ => throw new Exception()//TODO implement
	}
	
	def |(o: JNumber) = this
	
	def unary_- = JInfinity
	
	override def toString = "__"
}

final class JInt(val v: Int) extends Finite(jINT) {

	import JReal._
	def signum = if (v < 0) Neg else if (v == 0) Neut else Pos
  
	def compareFinite(r: Finite): Int = r match {
	  case i:JInt => v.compare(i.v)
	  case f:JFloat=> -(f.v compare v)
	}
    
    def +~(o: Finite):JNumber = o match {
      case i:JInt => new JInt(v + i.v)
      case f:JFloat=> new JFloat(v + f.v)
    }
    
    
    def -~(o: Finite):JNumber = o match {
      case i:JInt => new JInt(v - i.v)
      case f:JFloat=>new JFloat(v - f.v)
    }
    
    def *~(o: Finite):JNumber = o match {
      case i:JInt => new JInt(v * i.v)
      case f:JFloat=>new JFloat(v * f.v)
    }
    
    def *(i: JInt): JInt = new JInt(this.v * i.v)
    
    def %~(o: Finite):JNumber = o match {
      case i: JInt => if (i.v == 0) this * JInfinity else new JFloat(v / i.v)
      case f: JFloat=>if (f.v == 0.0)this *JInfinity else new JFloat(v / f.v)
    }
    
    def **~(o: Finite) = o match {
      case f:JFloat => new JFloat(math.pow(v.toDouble, f.v))
      case i:JInt => new JFloat(math.pow(v, i.v))
    }
    
    def %%~(o: Finite) = o match {
      case f: JFloat => new JFloat(math.log(f.v) / math.log(v))
      case i: JInt   => new JFloat(math.log(i.v) / math.log(v))
    }
    
    def |~(o: Finite) = o match {
      case f: JFloat => new JFloat(f.v % v)
      case i: JInt   => new JFloat(i.v % v)
    }
    
    def |(i: JInt) = new JInt(i.v % this.v)
    
    def unary_- = new JInt(-v)
    def unary_| = new JInt(v.abs)
    
    override def toString = v.toString
}

final class JFloat(val v: Double) extends Finite(jFL) {

	def compareFinite(r: Finite): Int = r match {
	  case i:JInt => v.compare(i.v)
	  case f:JFloat=>v.compare(f.v)
	}
	
	override def signum = if (v < 0) Neg else if (v == 0.0) Neut else Pos
	
	def +~(o: Finite):JNumber = o match {
	  case i: JInt => new JFloat(v + i.v)
	  case f: JFloat=>new JFloat(v + f.v)
	}
	
	def -~(o: Finite):JNumber = o match {
	  case i: JInt => new JFloat(v + i.v)
	  case f: JFloat=>new JFloat(v + f.v)
	}
	
	def *~(o: Finite):JNumber = o match {
	  case i: JInt => new JFloat(v + i.v)
	  case f: JFloat=>new JFloat(v + f.v)
	}
	
	def %~(o: Finite):JNumber = o match {
	  case i: JInt => new JFloat(v / i.v)
	  case f: JFloat=>new JFloat(v / f.v)
	}
	
	def **~(o: Finite): JNumber = o match {
	  case i: JInt => new JFloat(math.pow(v, i.v))
	  case f: JFloat => new JFloat(math.pow(v, f.v))
	}
	
	def %%~(o: Finite): JNumber = o match {
	  case i: JInt => new JFloat(math.log(i.v) / math.log(v))
	  case f: JFloat=> new JFloat(math.log(f.v) / math.log(v))
	}
	
	def |~(o: Finite): JNumber = o match {
	  case i: JInt => new JFloat(i.v % v)
	  case f: JFloat => new JFloat(f.v % v)
	}
	
	def unary_- = new JFloat(-v)
	def unary_| = new JFloat(v.abs)
	
	override def toString = v.toString
}
}