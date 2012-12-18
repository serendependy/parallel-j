import scala.math.max
import scala.math.{Numeric => OldNumeric, Integral, Fractional, min, max}
import scala.util.Random
import scala.testing.Benchmark

import java.io.{FileWriter, PrintWriter}

import Console.printf

import com.azavea.math.Numeric
import com.azavea.math.EasyImplicits._
//import com.azavea.math.FastImplicits._
import Predef.{any2stringadd => _, _}

// define some constant sizes and random arrays that we can use for our various
// performance tests. if things run way too slow or way too fast you can try
// changing the factor or divisor :)
object Constant {
  val factor = 1
  val divisor = 1

  val smallSize:Int = (10 * 1000 * factor) / divisor
  val smallIntArray = Array.ofDim[Int](smallSize).map(i => Random.nextInt())
  val smallLongArray = Array.ofDim[Long](smallSize).map(i => Random.nextLong())
  val smallFloatArray = Array.ofDim[Float](smallSize).map(i => Random.nextFloat())
  val smallDoubleArray = Array.ofDim[Double](smallSize).map(i => Random.nextDouble())

  val mediumSize:Int = (1 * 1000 * 1000 * factor) / divisor
  val mediumIntArray = Array.ofDim[Int](mediumSize).map(i => Random.nextInt())
  val mediumLongArray = Array.ofDim[Long](mediumSize).map(i => Random.nextLong())
  val mediumFloatArray = Array.ofDim[Float](mediumSize).map(i => Random.nextFloat())
  val mediumDoubleArray = Array.ofDim[Double](mediumSize).map(i => Random.nextDouble())

  val largeSize:Int = (10 * 1000 * 1000 * factor) / divisor
  val largeIntArray = Array.ofDim[Int](largeSize).map(i => Random.nextInt())
  val largeLongArray = Array.ofDim[Long](largeSize).map(i => Random.nextLong())
  val largeFloatArray = Array.ofDim[Float](largeSize).map(i => Random.nextFloat())
  val largeDoubleArray = Array.ofDim[Double](largeSize).map(i => Random.nextDouble())

  val createHTML = true
}
import Constant._

case class TestResult(tavg:Double, tmax:Long, tmin:Long)

// represents a particular performance test we want to run
trait TestCase {
  def name: String

  // direct implementation using primitives
  def direct(): Option[Any]

  // implemented using the new Numeric trait
  def newGeneric(): Option[Any]

  // implemented using the built-in Numeric trait
  def oldGeneric(): Option[Any]

  object CaseDirect extends Benchmark { def run = direct }
  object CaseGeneric extends Benchmark { def run = newGeneric }
  object CaseOld extends Benchmark { def run = oldGeneric }

  val cases = List(CaseDirect, CaseGeneric, CaseOld)

  def runCase(c:Benchmark, warmupRuns:Int, liveRuns:Int) = {
    c.runBenchmark(warmupRuns)
    val results = c.runBenchmark(liveRuns)

    val total = results.foldLeft(0L)(_ + _)

    val tmax = results.foldLeft(0L)(max(_, _))
    val tmin = results.foldLeft(Long.MaxValue)(min(_, _))
    val tavg = total.toDouble / liveRuns

    TestResult(tavg, tmax, tmin)
  }

  def run(n:Int, m:Int):List[TestResult] = cases.map(runCase(_, n, m))

  def percStatus(p:Double) = if (p < 0.9) "great"
  else if (p < 1.1) "good"
  else if (p < 2.2) "ok"
  else if (p < 4.4) "poor"
  else if (p < 8.8) "bad"
  else "awful"

  def test(p:Option[PrintWriter]) {
    val results = run(2, 8)

    val times = results.map(_.tavg)
    val tstrs = times.map {
      t => if (t < 0.1) {
        "     n/a"
      } else {
        "%6.1fms".format(t)
      }
    }

    val List(t1, t2, t3) = times

    def mkp(a:Double, b:Double) = if (a == 0.0 || b == 0.0) 0.0 else a / b

    val percs = List(mkp(t2, t1), mkp(t3, t1), mkp(t3, t2))
    val pstrs = percs.map(p => if(p < 0.01) "   n/a" else "%5.2fx".format(p))

    p match {
      case Some(pw) => {
        val a = "  <tr><td class='name'>%s</td><td class='base'>%.1f</td>".format(name, times(0))
        val s1 = percStatus(percs(0))
        val b = if (times(1) < 0.1) {
          "<td class='na'></td><td class='na'></td>"
        } else {
          "<td class='%s'>%.1f</td><td class='%s'>%s</td>".format(s1, times(1), s1, pstrs(0))
        }
        val s2 = percStatus(percs(1))
        val c = if (times(2) < 0.1) {
          "<td class='na'></td><td class='na'></td>"
        } else {
          "<td class='%s'>%.1f</td><td class='%s'>%s</td></tr>".format(s2, times(2), s2, pstrs(1))
        }
        pw.println(a + b + c)
      }
      case _ => {}
    }

    val fields = ("%-24s".format(name) :: tstrs) ++ ("/" :: pstrs)
    println(fields.reduceLeft(_ + "  " + _))
  }
}


// ===============================================================
trait FromIntToX extends TestCase {
  def directToInt(a:Array[Int]) = {
    val b = Array.ofDim[Int](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toInt
      i += 1
    }
    b
  }
  def directToLong(a:Array[Int]) = {
    val b = Array.ofDim[Long](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toLong
      i += 1
    }
    b
  }

  def directToFloat(a:Array[Int]) = {
    val b = Array.ofDim[Float](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toFloat
      i += 1
    }
    b
  }

  def directToDouble(a:Array[Int]) = {
    val b = Array.ofDim[Double](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toDouble
      i += 1
    }
    b
  }

  def newFromInts[@specialized A:Numeric:Manifest](a:Array[Int]): Array[A] = {
    val b = Array.ofDim[A](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = numeric.fromInt(a(i))
      i += 1
    }
    b
  }

  def oldFromInts[A:OldNumeric:Manifest](a:Array[Int]): Array[A] = {
    val m = implicitly[OldNumeric[A]]
    val b = Array.ofDim[A](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = m.fromInt(a(i))
      i += 1
    }
    b
  }
}

final class FromIntToInt extends FromIntToX {
  def name = "from-int-to-int"
  def direct() = Option(directToInt(largeIntArray))
  def newGeneric() = Option(newFromInts[Int](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Int](largeIntArray))
}

final class FromIntToLong extends FromIntToX {
  def name = "from-int-to-long"
  def direct() = Option(directToLong(largeIntArray))
  def newGeneric() = Option(newFromInts[Long](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Long](largeIntArray))
}

final class FromIntToFloat extends FromIntToX {
  def name = "from-int-to-float"
  def direct() = Option(directToFloat(largeIntArray))
  def newGeneric() = Option(newFromInts[Float](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Float](largeIntArray))
}

final class FromIntToDouble extends FromIntToX {
  def name = "from-int-to-double"
  def direct() = Option(directToDouble(largeIntArray))
  def newGeneric() = Option(newFromInts[Double](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Double](largeIntArray))
}



// =================================================================
trait BaseAdder extends TestCase {
  def newAdder[@specialized A](a:A, b:A)(implicit m:Numeric[A]): A
  def oldAdder[A](a:A, b:A)(implicit m:OldNumeric[A]): A

  def directIntAdder(a:Int, b:Int):Int = a + b
  def directLongAdder(a:Long, b:Long):Long = a + b
  def directFloatAdder(a:Float, b:Float):Float = a + b
  def directDoubleAdder(a:Double, b:Double):Double = a + b
}

trait BaseAdderInt extends BaseAdder {
  def direct() = {
    var s = 0
    var i = 0
    while (i < largeSize) { s = directIntAdder(s, i); i += 1 }
    Option(s)
  }
  def newGeneric() = {
    var s = 0
    var i = 0
    while (i < largeSize) { s = newAdder(s, i); i += 1 }
    Option(s)
  }
  def oldGeneric() = {
    var s = 0
    var i = 0
    while (i < largeSize) { s = oldAdder(s, i); i += 1 }
    Option(s)
  }
}

trait BaseAdderLong extends BaseAdder {
  def direct() = {
    var s = 0L
    var i = 0
    while (i < largeSize) { s = directLongAdder(s, i); i += 1 }
    Option(s)
  }
  def newGeneric() = {
    var s = 0L
    var i = 0
    while (i < largeSize) { s = newAdder(s, i); i += 1 }
    Option(s)
  }
  def oldGeneric() = {
    var s = 0L
    var i = 0
    while (i < largeSize) { s = oldAdder(s, i); i += 1 }
    Option(s)
  }
}

trait BaseAdderFloat extends BaseAdder {
  def direct() = {
    var s = 0.0F
    var i = 0
    while (i < largeSize) { s = directFloatAdder(s, i); i += 1 }
    Option(s)
  }
  def newGeneric() = {
    var s = 0.0F
    var i = 0
    while (i < largeSize) { s = newAdder(s, i); i += 1 }
    Option(s)
  }
  def oldGeneric() = {
    var s = 0.0F
    var i = 0
    while (i < largeSize) { s = oldAdder(s, i); i += 1 }
    Option(s)
  }
}

trait BaseAdderDouble extends BaseAdder {
  def direct() = {
    var s = 0.0
    var i = 0
    while (i < largeSize) { s = directDoubleAdder(s, i); i += 1 }
    Option(s)
  }
  def newGeneric() = {
    var s = 0.0
    var i = 0
    while (i < largeSize) { s = newAdder(s, i); i += 1 }
    Option(s)
  }
  def oldGeneric() = {
    var s = 0.0
    var i = 0
    while (i < largeSize) { s = oldAdder(s, i); i += 1 }
    Option(s)
  }
}


// =========================================================
trait Adder extends BaseAdder {
  def newAdder[@specialized A](a:A, b:A)(implicit m:Numeric[A]): A = m.plus(a, b)
  def oldAdder[A](a:A, b:A)(implicit m:OldNumeric[A]): A = m.plus(a, b)
}

final class AdderInt extends Adder with BaseAdderInt { def name = "adder-int" }
final class AdderLong extends Adder with BaseAdderLong { def name = "adder-long" }
final class AdderFloat extends Adder with BaseAdderFloat { def name = "adder-float" }
final class AdderDouble extends Adder with BaseAdderDouble{ def name = "adder-double" }


// =====================================================
trait BaseArrayOps extends TestCase {
  def directIntArrayOp(a:Int, b:Int): Int
  def directIntArrayOps(a:Array[Int]) = {
    var total = 0
    var i = 0
    while (i < a.length) {
      total = directIntArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def directLongArrayOp(a:Long, b:Long): Long
  def directLongArrayOps(a:Array[Long]) = {
    var total = 0L
    var i = 0
    while (i < a.length) {
      total = directLongArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def directFloatArrayOp(a:Float, b:Float): Float
  def directFloatArrayOps(a:Array[Float]) = {
    var total = 0.0F
    var i = 0
    while (i < a.length) {
      total = directFloatArrayOp(total, a(i))
      i += 1
    }
    total
  }
    
  def directDoubleArrayOp(a:Double, b:Double): Double
  def directDoubleArrayOps(a:Array[Double]) = {
    var total = 0.0
    var i = 0
    while (i < a.length) {
      total = directDoubleArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def newArrayOp[@specialized A:Numeric](a:A, b:A): A
  def newArrayOps[@specialized A:Numeric:Manifest](a:Array[A]) = {
    var total = numeric.zero
    var i = 0
    while (i < a.length) {
      total = newArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def oldArrayOp[A](a:A, b:A)(implicit m:OldNumeric[A]): A
  def oldArrayOps[A:OldNumeric:Manifest](a:Array[A]) = {
    val m = implicitly[OldNumeric[A]]
    var total = m.zero
    var i = 0
    while (i < a.length) {
      total = oldArrayOp(total, a(i))
      i += 1
    }
    total
  }
}


trait BaseArrayMapOps extends TestCase {
  def directIntArrayOp(a:Int): Int
  def directIntArrayOps(a:Array[Int]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directIntArrayOp(a(i))
      i += 1
    }
    a
  }

  def directLongArrayOp(a:Long): Long
  def directLongArrayOps(a:Array[Long]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directLongArrayOp(a(i))
      i += 1
    }
    a
  }

  def directFloatArrayOp(a:Float): Float
  def directFloatArrayOps(a:Array[Float]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directFloatArrayOp(a(i))
      i += 1
    }
    a
  }
    
  def directDoubleArrayOp(a:Double): Double
  def directDoubleArrayOps(a:Array[Double]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directDoubleArrayOp(a(i))
      i += 1
    }
    a
  }

  def newArrayOp[@specialized A:Numeric](a:A): A
  def newArrayOps[@specialized A:Numeric:Manifest](a:Array[A]) = {
    var i = 0
    while (i < a.length) {
      a(i) = newArrayOp(a(i))
      i += 1
    }
    a
  }

  def oldArrayOp[A](a:A)(implicit m:OldNumeric[A]): A
  def oldArrayOps[A:OldNumeric:Manifest](a:Array[A]) = {
    val m = implicitly[OldNumeric[A]]
    var i = 0
    while (i < a.length) {
      a(i) = oldArrayOp(a(i))
      i += 1
    }
    a
  }
}


// ======================================================
trait ArrayAdder extends BaseArrayOps {
  def directIntArrayOp(a:Int, b:Int) = a + b
  def directLongArrayOp(a:Long, b:Long) = a + b
  def directFloatArrayOp(a:Float, b:Float) = a + b
  def directDoubleArrayOp(a:Double, b:Double) = a + b

  def newArrayOp[@specialized A:Numeric](a:A, b:A) = numeric.plus(a, b)
  def oldArrayOp[A](a:A, b:A)(implicit m:OldNumeric[A]) = m.plus(a, b)
}

final class IntArrayAdder extends ArrayAdder {
  def name = "array-total-int"
  def direct() = Option(directIntArrayOps(largeIntArray))
  def newGeneric() = Option(newArrayOps(largeIntArray))
  def oldGeneric() = Option(oldArrayOps(largeIntArray))
}

final class LongArrayAdder extends ArrayAdder {
  def name = "array-total-long"
  def direct() = Option(directLongArrayOps(largeLongArray))
  def newGeneric() = Option(newArrayOps(largeLongArray))
  def oldGeneric() = Option(oldArrayOps(largeLongArray))
}

final class FloatArrayAdder extends ArrayAdder {
  def name = "array-total-float"
  def direct() = Option(directFloatArrayOps(largeFloatArray))
  def newGeneric() = Option(newArrayOps(largeFloatArray))
  def oldGeneric() = Option(oldArrayOps(largeFloatArray))
}

final class DoubleArrayAdder extends ArrayAdder {
  def name = "array-total-double"
  def direct() = Option(directDoubleArrayOps(largeDoubleArray))
  def newGeneric() = Option(newArrayOps(largeDoubleArray))
  def oldGeneric() = Option(oldArrayOps(largeDoubleArray))
}




// ==========================================================================
trait ArrayRescale extends BaseArrayMapOps {
  def directIntArrayOp(b:Int) = (b * 5) / 3
  def directLongArrayOp(b:Long) = (b * 5L) / 3L
  def directFloatArrayOp(b:Float) = (b * 5.0F) / 3.0F
  def directDoubleArrayOp(b:Double) = (b * 5.0) / 3.0

  def newArrayOp[@specialized A](b:A)(implicit m:Numeric[A]) = m.div(m.times(b, m.fromDouble(5.0)), m.fromDouble(3.0))
  def oldArrayOp[A](b:A)(implicit m:OldNumeric[A]) = m.fromInt((m.toDouble(b) * 5.0 / 3.0).toInt)
}

final class IntArrayRescale extends ArrayRescale {
  def name = "array-rescale-int"
  def direct() = Option(directIntArrayOps(largeIntArray))
  def newGeneric() = Option(newArrayOps(largeIntArray))
  def oldGeneric() = Option(oldArrayOps(largeIntArray))
}

final class LongArrayRescale extends ArrayRescale {
  def name = "array-rescale-long"
  def direct() = Option(directLongArrayOps(largeLongArray))
  def newGeneric() = Option(newArrayOps(largeLongArray))
  def oldGeneric() = Option(oldArrayOps(largeLongArray))
}

final class FloatArrayRescale extends ArrayRescale {
  def name = "array-rescale-float"
  def direct() = Option(directFloatArrayOps(largeFloatArray))
  def newGeneric() = Option(newArrayOps(largeFloatArray))
  def oldGeneric() = Option(oldArrayOps(largeFloatArray))
}

final class DoubleArrayRescale extends ArrayRescale {
  def name = "array-rescale-double"
  def direct() = Option(directDoubleArrayOps(largeDoubleArray))
  def newGeneric() = Option(newArrayOps(largeDoubleArray))
  def oldGeneric() = Option(oldArrayOps(largeDoubleArray))
}



// ==========================================================================
trait InfixAdder extends BaseAdder {
  def newAdder[@specialized A:Numeric](a:A, b:A): A = a + b
  def oldAdder[A](a:A, b:A)(implicit m:OldNumeric[A]): A = {
    import m._
    a + b
  }
}

final class InfixAdderInt extends InfixAdder with BaseAdderInt { def name = "infix-adder-int" }
final class InfixAdderLong extends InfixAdder with BaseAdderLong { def name = "infix-adder-long" }
final class InfixAdderFloat extends InfixAdder with BaseAdderFloat { def name = "infix-adder-float" }
final class InfixAdderDouble extends InfixAdder with BaseAdderDouble{ def name = "infix-adder-double" }

// ==========================================================
trait FindMax extends TestCase {
  def directMaxInt(a:Array[Int]) = {
    var curr = a(0)
    var i = 1
    while (i < a.length) { curr = scala.math.max(curr, a(i)); i += 1 }
    curr
  }

  def directMaxLong(a:Array[Long]) = {
    var curr = a(0)
    var i = 1
    while (i < a.length) { curr = scala.math.max(curr, a(i)); i += 1 }
    curr
  }

  def directMaxFloat(a:Array[Float]) = {
    var curr = a(0)
    var i = 1
    while (i < a.length) { curr = scala.math.max(curr, a(i)); i += 1 }
    curr
  }

  def directMaxDouble(a:Array[Double]) = {
    var curr = a(0)
    var i = 1
    while (i < a.length) { curr = scala.math.max(curr, a(i)); i += 1 }
    curr
  }

  def newGenericMax[@specialized A:Numeric](a:Array[A]) = {
    var curr = a(0)
    var i = 1
    while (i < a.length) { curr = numeric.max(curr, a(i)); i += 1 }
    curr
  }

  def oldGenericMax[A:OldNumeric](a:Array[A]) = {
    val n = implicitly[OldNumeric[A]]
    var curr = a(0)
    var i = 1
    while (i < a.length) { curr = n.max(curr, a(i)); i += 1 }
    curr
  }
}

final class FindMaxInt extends FindMax {
  def name = "find-max-int"
  def direct() = Some(directMaxInt(largeIntArray))
  def newGeneric() = Some(newGenericMax(largeIntArray))
  def oldGeneric() = Some(oldGenericMax(largeIntArray))
}

final class FindMaxLong extends FindMax {
  def name = "find-max-long"
  def direct() = Some(directMaxLong(largeLongArray))
  def newGeneric() = Some(newGenericMax(largeLongArray))
  def oldGeneric() = Some(oldGenericMax(largeLongArray))
}

final class FindMaxFloat extends FindMax {
  def name = "find-max-float"
  def direct() = Some(directMaxFloat(largeFloatArray))
  def newGeneric() = Some(newGenericMax(largeFloatArray))
  def oldGeneric() = Some(oldGenericMax(largeFloatArray))
}

final class FindMaxDouble extends FindMax {
  def name = "find-max-double"
  def direct() = Some(directMaxDouble(largeDoubleArray))
  def newGeneric() = Some(newGenericMax(largeDoubleArray))
  def oldGeneric() = Some(oldGenericMax(largeDoubleArray))
}

// ================================================================
trait BaseSort extends TestCase {
  def directIntSorter(a:Array[Int]): Array[Int]
  def directLongSorter(a:Array[Long]): Array[Long]
  def directFloatSorter(a:Array[Float]): Array[Float]
  def directDoubleSorter(a:Array[Double]): Array[Double]

  def newGenericSorter[@specialized A:Numeric:Manifest](a:Array[A]): Array[A]
  def oldGenericSorter[A:OldNumeric:Manifest](a:Array[A]): Array[A]

  def directInt(a:Array[Int]) = Option(directIntSorter(a))
  def directLong(a:Array[Long]) = Option(directLongSorter(a))
  def directFloat(a:Array[Float]) = Option(directFloatSorter(a))
  def directDouble(a:Array[Double]) = Option(directDoubleSorter(a))

  def newGenericSort[@specialized A:Numeric:Manifest](a:Array[A]) = Option(newGenericSorter(a))
  def oldGenericSort[A:OldNumeric:Manifest](a:Array[A]) = Option(oldGenericSorter(a))
}


// =======================================================================
trait Quicksort extends BaseSort {
  def directIntSorter(a:Array[Int]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
  def directLongSorter(a:Array[Long]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
  def directFloatSorter(a:Array[Float]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
  def directDoubleSorter(a:Array[Double]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }

  // NOTE: this will perform slowly just because Ordering is not specialized!
  def newGenericSorter[@specialized A:Numeric:Manifest](a:Array[A]) = {
    val d = a.clone;
    implicit val ord = implicitly[Numeric[A]].getOrdering()
    scala.util.Sorting.quickSort(d);
    d
  }
  def oldGenericSorter[A:OldNumeric:Manifest](a:Array[A]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
}

final class QuicksortInt extends Quicksort {
  def name = "quicksort-int"
  def direct() = directInt(mediumIntArray)
  def newGeneric() = newGenericSort(mediumIntArray)
  def oldGeneric() = oldGenericSort(mediumIntArray)
}

final class QuicksortLong extends Quicksort {
  def name = "quicksort-long"
  def direct() = directLong(mediumLongArray)
  def newGeneric() = newGenericSort(mediumLongArray)
  def oldGeneric() = oldGenericSort(mediumLongArray)
}

final class QuicksortFloat extends Quicksort {
  def name = "quicksort-float"
  def direct() = directFloat(mediumFloatArray)
  def newGeneric() = newGenericSort(mediumFloatArray)
  def oldGeneric() = oldGenericSort(mediumFloatArray)
}

final class QuicksortDouble extends Quicksort {
  def name = "quicksort-double"
  def direct() = directDouble(mediumDoubleArray)
  def newGeneric() = newGenericSort(mediumDoubleArray)
  def oldGeneric() = oldGenericSort(mediumDoubleArray)
}

// ==========================================
trait InsertionSort extends BaseSort {
  def directIntSorter(b:Array[Int]) = {
    val a = b.clone
    var i = 0
    while (i < a.length - 1) {
      var j = i + 1
      var k = i
      while (j < a.length) {
        if (a(j) < a(i)) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
    a
  }
  def directLongSorter(b:Array[Long]) = {
    val a = b.clone
    var i = 0
    while (i < a.length - 1) {
      var j = i + 1
      var k = i
      while (j < a.length) {
        if (a(j) < a(i)) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
    a
  }
  def directFloatSorter(b:Array[Float]) = {
    val a = b.clone
    var i = 0
    while (i < a.length - 1) {
      var j = i + 1
      var k = i
      while (j < a.length) {
        if (a(j) < a(i)) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
    a
  }
  def directDoubleSorter(b:Array[Double]) = {
    val a = b.clone
    var i = 0
    while (i < a.length - 1) {
      var j = i + 1
      var k = i
      while (j < a.length) {
        if (a(j) < a(i)) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
    a
  }

  def newGenericSorter[@specialized A:Numeric:Manifest](b:Array[A]) = {
    val a = b.clone
    var i = 0
    while (i < a.length - 1) {
      var j = i + 1
      var k = i
      while (j < a.length) {
        if (numeric.lt(a(j), a(i))) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
    a
  }  

  def oldGenericSorter[A:OldNumeric:Manifest](b:Array[A]) = {
    val n = implicitly[OldNumeric[A]]
    val a = b.clone
    var i = 0
    while (i < a.length - 1) {
      var j = i + 1
      var k = i
      while (j < a.length) {
        if (n.lt(a(j), a(i))) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
    a
  }  
}

final class InsertionSortInt extends InsertionSort {
  def name = "insertion-sort-int"
  def direct() = directInt(smallIntArray)
  def newGeneric() = newGenericSort(smallIntArray)
  def oldGeneric() = oldGenericSort(smallIntArray)
}

final class InsertionSortLong extends InsertionSort {
  def name = "insertion-sort-long"
  def direct() = directLong(smallLongArray)
  def newGeneric() = newGenericSort(smallLongArray)
  def oldGeneric() = oldGenericSort(smallLongArray)
}

final class InsertionSortFloat extends InsertionSort {
  def name = "insertion-sort-float"
  def direct() = directFloat(smallFloatArray)
  def newGeneric() = newGenericSort(smallFloatArray)
  def oldGeneric() = oldGenericSort(smallFloatArray)
}

final class InsertionSortDouble extends InsertionSort {
  def name = "insertion-sort-double"
  def direct() = directDouble(smallDoubleArray)
  def newGeneric() = newGenericSort(smallDoubleArray)
  def oldGeneric() = oldGenericSort(smallDoubleArray)
}


// ========================================================
trait ArrayAllocator extends TestCase {
  def directIntAllocator(num:Int, dim:Int, const:Int) = {
    val outer = Array.ofDim[Array[Int]](num)
    var i = 0
    while (i < num) { outer(i) = Array.fill(dim)(const); i += 1 }
    outer
  }

  def directLongAllocator(num:Int, dim:Int, const:Long) = {
    val outer = Array.ofDim[Array[Long]](num)
    var i = 0
    while (i < num) { outer(i) = Array.fill(dim)(const); i += 1 }
    outer
  }

  def directFloatAllocator(num:Int, dim:Int, const:Float) = {
    val outer = Array.ofDim[Array[Float]](num)
    var i = 0
    while (i < num) { outer(i) = Array.fill(dim)(const); i += 1 }
    outer
  }

  def directDoubleAllocator(num:Int, dim:Int, const:Double) = {
    val outer = Array.ofDim[Array[Double]](num)
    var i = 0
    while (i < num) { outer(i) = Array.fill(dim)(const); i += 1 }
    outer
  }

  def newAllocator[@specialized A:Numeric:Manifest](num:Int, dim:Int, const:A) = {
    val outer = Array.ofDim[Array[A]](num)
    var i = 0
    while (i < num) { outer(i) = Array.fill(dim)(const); i += 1 }
    outer
  }

  def oldAllocator[A:OldNumeric:Manifest](num:Int, dim:Int, const:A) = {
    val outer = Array.ofDim[Array[A]](num)
    var i = 0
    while (i < num) { outer(i) = Array.fill(dim)(const); i += 1 }
    outer
  }
}

final class ArrayAllocatorInt extends ArrayAllocator {
  def name = "array-allocator-int"
  def direct = Option(directIntAllocator(mediumSize, 5, 13))
  def newGeneric = Option(newAllocator(mediumSize, 5, 13))
  def oldGeneric = Option(oldAllocator(mediumSize, 5, 13))
}

final class ArrayAllocatorLong extends ArrayAllocator {
  def name = "array-allocator-long"
  def direct = Option(directLongAllocator(mediumSize, 5, 13L))
  def newGeneric = Option(newAllocator(mediumSize, 5, 13L))
  def oldGeneric = Option(oldAllocator(mediumSize, 5, 13L))
}

final class ArrayAllocatorFloat extends ArrayAllocator {
  def name = "array-allocator-float"
  def direct = Option(directFloatAllocator(mediumSize, 5, 13.0F))
  def newGeneric = Option(newAllocator(mediumSize, 5, 13.0F))
  def oldGeneric = Option(oldAllocator(mediumSize, 5, 13.0F))
}

final class ArrayAllocatorDouble extends ArrayAllocator {
  def name = "array-allocator-double"
  def direct = Option(directDoubleAllocator(mediumSize, 5, 13.0))
  def newGeneric = Option(newAllocator(mediumSize, 5, 13.0))
  def oldGeneric = Option(oldAllocator(mediumSize, 5, 13.0))
}

// =================================================================
trait MergeSort extends BaseSort {
  def directIntSorter(a:Array[Int]) = {
    if (a.length > 1) {
      val llen = a.length / 2
      val rlen = a.length - llen

      val left  = Array.ofDim[Int](llen)
      Array.copy(a, 0, left, 0, llen)
      directIntSorter(left)
  
      val right = Array.ofDim[Int](rlen)
      Array.copy(a, llen, right, 0, rlen)
      directIntSorter(right)

      var (i, j, k) = (0, 0, 0)
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i); i += 1
        } else if (i == llen) {
          a(k) = right(j); j += 1
        } else if (left(i) < right(j)) {
          a(k) = left(i); i += 1
        } else {
          a(k) = right(j); j += 1
        }
        k += 1
      }
    }
    a
  }

  def directLongSorter(a:Array[Long]) = {
    if (a.length > 1) {
      val llen = a.length / 2
      val rlen = a.length - llen

      val left  = Array.ofDim[Long](llen)
      Array.copy(a, 0, left, 0, llen)
      directLongSorter(left)
  
      val right = Array.ofDim[Long](rlen)
      Array.copy(a, llen, right, 0, rlen)
      directLongSorter(right)

      var (i, j, k) = (0, 0, 0)
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i); i += 1
        } else if (i == llen) {
          a(k) = right(j); j += 1
        } else if (left(i) < right(j)) {
          a(k) = left(i); i += 1
        } else {
          a(k) = right(j); j += 1
        }
        k += 1
      }
    }
    a
  }

  def directFloatSorter(a:Array[Float]) = {
    if (a.length > 1) {
      val llen = a.length / 2
      val rlen = a.length - llen

      val left  = Array.ofDim[Float](llen)
      Array.copy(a, 0, left, 0, llen)
      directFloatSorter(left)
  
      val right = Array.ofDim[Float](rlen)
      Array.copy(a, llen, right, 0, rlen)
      directFloatSorter(right)

      var (i, j, k) = (0, 0, 0)
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i); i += 1
        } else if (i == llen) {
          a(k) = right(j); j += 1
        } else if (left(i) < right(j)) {
          a(k) = left(i); i += 1
        } else {
          a(k) = right(j); j += 1
        }
        k += 1
      }
    }
    a
  }

  def directDoubleSorter(a:Array[Double]) = {
    if (a.length > 1) {
      val llen = a.length / 2
      val rlen = a.length - llen

      val left  = Array.ofDim[Double](llen)
      Array.copy(a, 0, left, 0, llen)
      directDoubleSorter(left)
  
      val right = Array.ofDim[Double](rlen)
      Array.copy(a, llen, right, 0, rlen)
      directDoubleSorter(right)

      var (i, j, k) = (0, 0, 0)
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i); i += 1
        } else if (i == llen) {
          a(k) = right(j); j += 1
        } else if (left(i) < right(j)) {
          a(k) = left(i); i += 1
        } else {
          a(k) = right(j); j += 1
        }
        k += 1
      }
    }
    a
  }

  def newGenericSorter[@specialized A:Numeric:Manifest](a:Array[A]):Array[A] = {
    if (a.length > 1) {
      val llen = a.length / 2
      val rlen = a.length - llen

      val left  = Array.ofDim[A](llen)
      Array.copy(a, 0, left, 0, llen)
      newGenericSorter(left)
  
      val right = Array.ofDim[A](rlen)
      Array.copy(a, llen, right, 0, rlen)
      newGenericSorter(right)

      var (i, j, k) = (0, 0, 0)
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i); i += 1
        } else if (i == llen) {
          a(k) = right(j); j += 1
        } else if (numeric.lt(left(i), right(j))) {
          a(k) = left(i); i += 1
        } else {
          a(k) = right(j); j += 1
        }
        k += 1
      }
    }
    a
  }

  def oldGenericSorter[A:OldNumeric:Manifest](a:Array[A]):Array[A] = {
    if (a.length > 1) {
      val n = implicitly[OldNumeric[A]]

      val llen = a.length / 2
      val rlen = a.length - llen

      val left  = Array.ofDim[A](llen)
      Array.copy(a, 0, left, 0, llen)
      oldGenericSorter(left)
  
      val right = Array.ofDim[A](rlen)
      Array.copy(a, llen, right, 0, rlen)
      oldGenericSorter(right)

      var (i, j, k) = (0, 0, 0)
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i); i += 1
        } else if (i == llen) {
          a(k) = right(j); j += 1
        } else if (n.lt(left(i), right(j))) {
          a(k) = left(i); i += 1
        } else {
          a(k) = right(j); j += 1
        }
        k += 1
      }
    }
    a
  }
}

final class MergeSortInt extends MergeSort {
  def name = "merge-sort-int"
  def direct() = { val a = mediumIntArray.clone; directInt(a); Some(a) }
  def newGeneric() = { val a = mediumIntArray.clone; newGenericSort(a); Some(a) }
  def oldGeneric() = { val a = mediumIntArray.clone; oldGenericSort(a); Some(a) }
}

final class MergeSortLong extends MergeSort {
  def name = "merge-sort-long"
  def direct() = { val a = mediumLongArray.clone; directLong(a); Some(a) }
  def newGeneric() = { val a = mediumLongArray; newGenericSort(a); Some(a) }
  def oldGeneric() = { val a = mediumLongArray; oldGenericSort(a); Some(a) }
}

final class MergeSortFloat extends MergeSort {
  def name = "merge-sort-float"
  def direct() = { val a = mediumFloatArray; directFloat(a); Some(a) }
  def newGeneric() = { val a = mediumFloatArray; newGenericSort(a); Some(a) }
  def oldGeneric() = { val a = mediumFloatArray; oldGenericSort(a); Some(a) }
}

final class MergeSortDouble extends MergeSort {
  def name = "merge-sort-double"
  def direct() = { val a = mediumDoubleArray; directDouble(a); Some(a) }
  def newGeneric() = { val a = mediumDoubleArray; newGenericSort(a); Some(a) }
  def oldGeneric() = { val a = mediumDoubleArray; oldGenericSort(a); Some(a) }
}


// =================================================================
final class IncrementInt1 extends TestCase {
  def name = "increment-int1"

  def directIncrement(x:Int) = x + 100
  def direct() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = directIncrement(total)
      i += 1
    }
    Some(total)
  }

  def newIncrement[@specialized A:Numeric](a:A):A = numeric.plus(a, numeric.fromInt(100))
  def newGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = newIncrement(total)
      i += 1
    }
    Some(total)
  }

  def oldIncrement[A](a:A)(implicit n:OldNumeric[A]) = n.plus(a, n.fromInt(100))
  def oldGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = oldIncrement(total)
      i += 1
    }
    Some(total)
  }
}

final class IncrementInt2 extends TestCase {
  def name = "increment-int2"

  def directIncrement(x:Int) = x + 100
  def direct() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = directIncrement(total)
      i += 1
    }
    Some(total)
  }

  def newIncrement[@specialized A:Numeric](a:A):A = a + numeric.fromInt(100)
  def newGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = newIncrement(total)
      i += 1
    }
    Some(total)
  }

  def oldIncrement[A](a:A)(implicit n:OldNumeric[A]) = n.plus(a, n.fromInt(100))
  def oldGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = oldIncrement(total)
      i += 1
    }
    Some(total)
  }
}

final class IncrementInt3 extends TestCase {
  def name = "increment-int3"

  def directIncrement(x:Int) = x + 100
  def direct() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = directIncrement(total)
      i += 1
    }
    Some(total)
  }

  def newIncrement[@specialized A:Numeric](a:A):A = a + 100
  def newGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = newIncrement(total)
      i += 1
    }
    Some(total)
  }

  def oldIncrement[A](a:A)(implicit n:OldNumeric[A]) = n.plus(a, n.fromInt(100))
  def oldGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = oldIncrement(total)
      i += 1
    }
    Some(total)
  }
}

final class IncrementInt4 extends TestCase {
  def name = "increment-int4"

  def directIncrement(x:Int) = x + 100
  def direct() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = directIncrement(total)
      i += 1
    }
    Some(total)
  }

  def newIncrement[@specialized A:Numeric](a:A):A = 100 + a
  def newGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = newIncrement(total)
      i += 1
    }
    Some(total)
  }

  def oldIncrement[A](a:A)(implicit n:OldNumeric[A]) = n.plus(a, n.fromInt(100))
  def oldGeneric() = {
    var i = 0
    var total = 0
    while (i < largeSize) {
      total = oldIncrement(total)
      i += 1
    }
    Some(total)
  }
}


object Main {
  val tests = List(List(new FromIntToInt,
                        new FromIntToLong,
                        new FromIntToFloat,
                        new FromIntToDouble),
                   
                   List(new InfixAdderInt,
                        new InfixAdderLong,
                        new InfixAdderFloat,
                        new InfixAdderDouble),
                   
                   List(new AdderInt,
                        new AdderLong,
                        new AdderFloat,
                        new AdderDouble),
                   
                   List(new IntArrayAdder,
                        new LongArrayAdder,
                        new FloatArrayAdder,
                        new DoubleArrayAdder),
                   
                   List(new IntArrayRescale,
                        new LongArrayRescale,
                        new FloatArrayRescale,
                        new DoubleArrayRescale),
                   
                   List(new ArrayAllocatorInt,
                        new ArrayAllocatorLong,
                        new ArrayAllocatorFloat,
                        new ArrayAllocatorDouble),
                   
                   List(new FindMaxInt,
                        new FindMaxLong,
                        new FindMaxFloat,
                        new FindMaxDouble),
                   
                   List(new QuicksortInt,
                        new QuicksortLong,
                        new QuicksortFloat,
                        new QuicksortDouble),
                   
                   List(new InsertionSortInt,
                        new InsertionSortLong,
                        new InsertionSortFloat,
                        new InsertionSortDouble),
                   
                   List(new MergeSortInt,
                        new MergeSortLong,
                        new MergeSortFloat,
                        new MergeSortDouble),
                   
                   List(new IncrementInt1,
                        new IncrementInt2,
                        new IncrementInt3,
                        new IncrementInt4))
                   
                   
  def getHTMLHeader() = """
<html>
 <head>
  <style type="text/css">
    td { text-align: right; padding: 4px; }
    td { border: 1px solid black; padding: 4px; }
    thead { background-color: lightgrey; }
    .na { border: 0px; }
    .base { border: 0px; }
    .sep { border: 0px; colspan="6" }
    .name { background-color: lightgrey; }
    .great { background-color: #99ccff; }
    .good { background-color: #99ff99; }
    .ok { background-color: #ccff99; }
    .poor { background-color: #ffff99; }
    .bad { background-color: #ffcc99; }
    .awful { background-color: #ff9999; }
  </style>
 </head>
 <body>
  <table>

  <thead>
   <tr><td>test</td><td>direct (ms)</td><td colspan="2">new (ms)</td><td colspan="2">old (ms)</td></tr>
  </thead>
  """

  def getHTMLFooter() = "  </table>\n </body>\n</html>\n"

  def main(args:Array[String]): Unit = {
    if (Constant.createHTML) { 
      println("creating benchmark.html...")
      printf("%-24s  %8s  %8s  %8s  /  %6s  %6s  %6s\n", "test", "direct", "new", "old", "n:d", "o:d", "o:n")

      val p = new PrintWriter(new FileWriter("benchmark.html"))

      p.println(getHTMLHeader())
      tests.foreach {
        group => {
          p.println("<tr><td class='sep' /></tr>\n")
          group.foreach(_.test(Some(p)))
        }
      }
      p.println(getHTMLFooter())
      p.close()

    } else {
      printf("%-24s  %8s  %8s  %8s  /  %6s  %6s  %6s\n", "test", "direct", "new", "old", "n:d", "o:d", "o:n")
      tests.foreach {
        group => group.foreach(_.test(None))
      }
    }
  }
}
