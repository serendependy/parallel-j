package j.lang.datatypes.array.types

import scala.math.Ordered

import j.lang.datatypes.array.JArrayType
import j.lang.datatypes.JTypeMacros._

import JReal._

sealed abstract class JNumber(jtype: JType) extends JArrayType(jtype){
	def +(o: JNumber): JNumber
	def -(o: JNumber): JNumber
	def *(o: JNumber): JNumber
	def %(o: JNumber): JNumber
	
	def unary_| : JNumber
	def unary_- : JNumber
	
	override def toString: String
}

object JNumber {
  def apply(a: Any) = a match {
    case i:Int => new JInt(i)
    case d:Double=> new JFloat(d)
    case _ => NaN
  }
}

object JReal {
  sealed abstract class Signum
  final object Neg extends Signum
  final object Neut extends Signum
  final object Pos extends Signum
  
  val Zero = new JInt(0)
  val One  = new JInt(1)
}

sealed abstract class JReal(jtype: JType) extends JNumber(jtype) with Ordered[JReal] {
  def signum: Signum
}

final object NaN extends JNumber(jFL){
  
  def compare(r: JReal) = 0
  
  def +(o: JNumber) = this
  def -(o: JNumber) = this
  def *(o: JNumber) = this
  def %(o: JNumber) = this

  def unary_- = this
  def unary_| = this
  
  override def toString = "_."
}

sealed abstract class Infinite extends JReal(jFL) with Ordered[JReal] {
  
  def %(o: JNumber) = o match {
    case fi: Finite => this
    case inf:Infinite=> NaN
    case NaN => NaN
  }
  
  def unary_| = Infinity
}

sealed abstract class Finite(jtype: JType) extends JReal(jtype) with Ordered[JReal]{
  def compareFinite(fi: Finite): Int
  
  def compare(r: JReal) = r match {
    case Infinity => -1
    case NegativeInfinity => 1
    case fi:Finite => compareFinite(fi)
  }
  
  protected def +~(fi: Finite): JNumber
  protected def -~(fi: Finite): JNumber
  protected def *~(fi: Finite): JNumber
  protected def %~(fi: Finite): JNumber
  
  def +(o: JNumber) = o match {
    case inf: Infinite => inf + o
    case fi: Finite => this +~ fi
    case NaN => NaN
  }
  
  def -(o: JNumber) = o match {
    case inf: Infinite => (-inf) + this
    case fi: Finite => this -~ fi
    case NaN => NaN
  }
  
  def *(o: JNumber) = o match {
    case inf: Infinite => inf * this
    case fi: Finite => this *~ fi
    case NaN => NaN
  }
  
  def %(o: JNumber) = o match {
    case inf: Infinite => Zero
    case fi: Finite => this %~ fi
    case NaN => NaN
  }
}


final object Infinity extends Infinite with Ordered[JReal] {
  import JReal._
  
  def signum = Pos
  
  def compare(r: JReal): Int = r match {
    case Infinity => 0
    case _ => -1
  }
  
  def +(o: JNumber) = o match {
    case NegativeInfinity => NaN //TODO NaN error
    case NaN => NaN
    case r:JReal => this
  }
  
  def -(o: JNumber) = o match {
    case Infinity => NaN //TODO NaN error
    case NaN => NaN
    case r:JReal => NegativeInfinity
  }
  
  def *(o: JNumber) = o match {
    case NaN => NaN
    case r:JReal => (r signum) match {
      case Pos => Infinity
      case Neut=> Zero
      case Neg => NegativeInfinity
    }
  }

  def unary_- = NegativeInfinity
  
  override def toString = "_"
}

final object NegativeInfinity extends Infinite with Ordered[JReal] {
	import JReal._
	
	def signum = Neg
	
	def compare(r: JReal): Int = r match {
	  case NegativeInfinity => 0
	  case _ => 1
	}
	
	def +(o: JNumber) = o match {
	  case Infinity => NaN //TODO NaN error
	  case NaN => NaN
	  case r:JReal => this
	}
	
	def -(o: JNumber) = o match {
	  case NegativeInfinity => NaN //TODO NaN error
	  case NaN => NaN
	  case r: JReal => Infinity
	}
	
	def *(o: JNumber) = o match {
	  case NaN => NaN
	  case r:JReal => (r signum) match {
	    case Pos => NegativeInfinity
	    case Neut=> Zero
	    case Neg => Infinity
	  }
	}

	def unary_- = Infinity
	
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
      case i: JInt => if (i.v == 0) this * Infinity else new JFloat(v / i.v)
      case f: JFloat=>if (f.v == 0.0)this *Infinity else new JFloat(v / f.v)
    }
    
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
	
	def unary_- = new JFloat(-v)
	def unary_| = new JFloat(v.abs)
	
	override def toString = v.toString
}