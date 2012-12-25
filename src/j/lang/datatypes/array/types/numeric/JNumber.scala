package j.lang.datatypes.array.types.numeric

import scala.math.Ordered

import JReal._
import com.sun.corba.se.spi.extension.ZeroPortPolicy

sealed abstract class JNumber {
	def +(o: JNumber): JNumber
	def -(o: JNumber): JNumber
	def *(o: JNumber): JNumber
	def /(o: JNumber): JNumber
	
	def unary_| : JNumber
	def unary_- : JNumber
	
	override def toString: String
}

object JReal {
  sealed class Signum
  final object Neg extends Signum
  final object Neut extends Signum
  final object Pos extends Signum
  
  val Zero = new JInt(0)
}

sealed abstract class JReal extends JNumber with Ordered[JReal] {
  def signum: Signum
}

trait Infinite

final object Infinity extends JReal with Ordered[JReal] with Infinite {
  import JReal._
  
  def signum = Pos
  
  def compare(r: JReal): Int = r match {
    case Infinity => 0
    case _ => -1
  }
  
  def +(o: JNumber) = o match {
    case NegativeInfinity => throw new Exception() //TODO NaN error
    case r:JReal => this
  }
  
  def -(o: JNumber) = o match {
    case Infinity => throw new Exception()
    case r:JReal => NegativeInfinity
  }
  
  def *(o: JNumber) = o match {
    case r:JReal => (r signum) match {
      case Pos => Infinity
      case Neut=> Zero
      case Neg => NegativeInfinity
    }
  }
  
  def /(o: JNumber) = o match {
    case e:Infinite => throw new Exception()
    case _ => this
  }
  
  def unary_| = this
  def unary_- = NegativeInfinity
  
  override def toString = "_"
}

final object NegativeInfinity extends JReal with Ordered[JReal] {
	import JReal._
	
	def signum = Neg
	
	def compare(r: JReal): Int = r match {
	  case NegativeInfinity => 0
	  case _ => 1
	}
	
	def +(o: JNumber) = o match {
	  case Infinity => throw new Exception() //TODO NaN error
	  case r:JReal => this
	}
	
	def -(o: JNumber) = o match {
	  case NegativeInfinity => throw new Exception()
	  case r: JReal => Infinity
	}
	
	def *(o: JNumber) = o match {
	  case r:JReal => (r signum) match {
	    case Pos => NegativeInfinity
	    case Neut=> Zero
	    case Neg => Infinity
	  }
	}
	
	def /(o: JNumber) = o match {
	  case e: Infinite => throw new Exception()
	  case _ => this
	}
	
	def unary_| = this
	def unary_- = Infinity
	
	override def toString = "__"
}

final class JInt(val v: Int) extends JReal with Ordered[JReal] {

	def compare(r: JReal): Int = r match {
	  case i:JInt => v.compare(i.v)
	  case f:JFloat=> -(f.v compare v)
	}
    
    def +(o: JNumber) = o match {
      case i:JInt => new JInt(v + i.v)
      case f:JFloat=> new JFloat(v + f.v)
    }
    
    def -(o: JNumber) = o match {
      case i:JInt => new JInt(v - i.v)
      case f:JFloat=>new JFloat(v - f.v)
    }
    
    def *(o: JNumber) = o match {
      case i:JInt => new JInt(v * i.v)
      case f:JFloat=>new JFloat(v * f.v)
    }
    
    def /(o: JNumber) = o match {
      case i: JInt => {
        if (v % i.v == 0) new JInt(v / i.v)
        else new JFloat(v.toDouble / i.v)
      }
    }
    
    def unary_- = new JInt(-v)
    def unary_| = new JInt(v.abs)
}

final class JFloat(val v: Double) extends JReal with Ordered[JReal] {

	def compare(r: JReal): Int = r match {
	  case i:JInt => v.compare(i.v)
	  case f:JFloat=>v.compare(f.v)
	}
	
	
}