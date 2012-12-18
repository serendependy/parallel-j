import com.azavea.math.Numeric
import com.azavea.math.EasyImplicits._
import Predef.{any2stringadd => _, _}

class MatrixException(s:String) extends Exception


/**
 * Matrix implementation in terms of a generic numeric type.
 */
class NMatrix[A:Numeric:Manifest](val data:Array[Array[A]],
                                 val rows:Int, val cols:Int) {
  if (rows < 1) throw new MatrixException("illegal height")
  if (cols < 1) throw new MatrixException("illegal widht")

  /* build an empty matrix with the same dimensions */
  def createEmpty() = NMatrix.empty[A](rows, cols)

  /* access the element at (y, x) */
  def apply(y:Int, x:Int) = data(y)(x)

  /* update the element at (y, x) to value */
  def update(y:Int, x:Int, value:A) = data(y)(x) = value

  /* create a new Matrix by mapping each element through f */
  def map[B:Numeric:Manifest](f:A => B) = {
    new NMatrix[B](data.map(_.map(f).toArray).toArray, rows, cols)
  }

  /* combine two matrices element-by-element */
  def combine(rhs:NMatrix[A], f:(A, A) => A) = {
    val result = createEmpty
    for (y <- 0 until rows; x <- 0 until cols) {
      result(y, x) = f(this(y, x), rhs(y, x))
    }
    result
  }

  /* add a scalar value to each element */
  def +(a:A):NMatrix[A] = map(_ + a)

  /* add two matrices */
  def +(rhs:NMatrix[A]):NMatrix[A] = combine(rhs, _ + _)

  /* multiply each element by a scalar value */
  def *(a:A):NMatrix[A] = map(_ * a)
  
  /* multiply two matrices */
  def *(rhs:NMatrix[A]):NMatrix[A] = {

    /* make sure this and rhs are compatible */
    if (this.rows != rhs.cols || this.cols != rhs.rows) {
      throw new MatrixException("dimensions do not match")
    }

    /* figure out the dimensions of the result matrix */
    val (rrows, rcols, n) = (this.rows, rhs.cols, this.cols)

    /* allocate the result matrix */
    val result = NMatrix.empty[A](rows, rcols)

    /* loop over the cells in the result matrix */
    for(y <- 0 until rrows; x <- 0 until rcols) {
      /* for each pair of values in this-row/rhs-column, multiply them
       * and then sum to get the result value for this cell. */
      result(y, x) = (0 until n).foldLeft(numeric.zero) {
        case (sum, i) => sum + (this(y, i) * rhs(i, x))
      }
    }
    result
  }

  def toAscii() = "[" + data.map {
    _.foldLeft("")(_ + " " + _.toString)
  }.reduceLeft(_ + "\n" + _) + "]"
}

object NMatrix {
  def empty[A:Numeric:Manifest](rows:Int, cols:Int) = {
    new NMatrix(Array.ofDim[A](rows, cols), rows, cols)
  }

  def apply[A:Numeric:Manifest](data:Array[Array[A]], rows:Int, cols:Int) = {
    new NMatrix(data, rows, cols)
  }
}

/**
 * Matrix implementation in terms of Double.
 */
class DMatrix(val data:Array[Array[Double]], val rows:Int, val cols:Int) {
  if (rows < 1) throw new MatrixException("illegal height")
  if (cols < 1) throw new MatrixException("illegal widht")

  /* build an empty matrix with the same dimensions */
  def createEmpty() = DMatrix.empty(rows, cols)

  /* access the element at (y, x) */
  def apply(y:Int, x:Int) = data(y)(x)

  /* update the element at (y, x) to value */
  def update(y:Int, x:Int, value:Double) = data(y)(x) = value

  /* create a new Matrix by mapping each element through f */
  def map(f:Double => Double) = {
    new DMatrix(data.map(_.map(f).toArray).toArray, rows, cols)
  }

  /* combine two matrices element-by-element */
  def combine(rhs:DMatrix, f:(Double, Double) => Double) = {
    val result = createEmpty
    for (y <- 0 until rows; x <- 0 until cols) {
      result(y, x) = f(this(y, x), rhs(y, x))
    }
    result
  }

  /* add a scalar value to each element */
  def +(a:Double) = map(_ + a)

  /* add two matrices */
  def +(rhs:DMatrix) = combine(rhs, _ + _)

  /* multiply each element by a scalar value */
  def *(a:Double) = map(_ * a)
  
  /* multiply two matrices */
  def *(rhs:DMatrix) = {

    /* make sure this and rhs are compatible */
    if (this.rows != rhs.cols || this.cols != rhs.rows) {
      throw new MatrixException("dimensions do not match")
    }

    val (rrows, rcols, n) = (this.rows, rhs.cols, this.cols)

    val result = DMatrix.empty(rrows, rcols)

    for(y <- 0 until rrows; x <- 0 until rcols) {
      result(y, x) = (0 until n).foldLeft(0.0) {
        case (sum, i) => sum + (this(y, i) * rhs(i, x))
      }
    }

    result
  }

  def toAscii() = "[" + data.map {
    _.foldLeft("")(_ + " " + _.toString)
  }.reduceLeft(_ + "\n" + _) + "]\n"
}

object DMatrix {
  def empty(rows:Int, cols:Int) = {
    new DMatrix(Array.ofDim[Double](rows, cols), rows, cols)
  }

  def apply(data:Array[Array[Double]], rows:Int, cols:Int) = {
    new DMatrix(data, rows, cols)
  }
}



/**
 * Testing...
 */
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class MatrixSpec extends FunSuite with ShouldMatchers {
  val (h, w) = (2, 2)
  val data1 = Array(Array(1.0, 2.0), Array(3.0, 4.0))

  def compare(m1:NMatrix[Double], m2:DMatrix):Boolean = {
    println(m1.toAscii)
    println(m2.toAscii)
    for (y <- 0 until h; x <- 0 until w) {
      if (m1(y, x) !== m2(y, x)) return false
    }
    true
  }

  val m1 = NMatrix(data1.clone, h, w)
  val m2 = DMatrix(data1.clone, h, w)
  
  test("test matrix representation") { assert(compare(m1, m2)) }

  test("scalar addition") { assert(compare(m1 + 13.0, m2 + 13.0)) }
  test("scalar multiplication") { assert(compare(m1 * 9.0, m2 * 9.0)) }

  test("matrix addition") { assert(compare(m1 + m1, m2 + m2)) }
  test("matrix multiplication") { assert(compare(m1 * m1, m2 * m2)) }
}
