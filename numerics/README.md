NUMERIC
=======

This package (com.azavea.math.Numeric) is based on scala.math.Numeric, which is
a type trait designed to allow abstraction across numeric types (e.g. Int,
Double, etc). I (Erik Osheim) began this work as part of the
specialized-numeric Scala Incubator project. 

This package is a much-improved version. For one, it is way faster, thanks to
Scala's specialization system, as well as some restructuring and an optional
compiler plugin. It is also more flexible, allowing the user to operate on
generic numeric types, concrete numeric types, and literals without requiring
ugly casting.

Ultimately my hope is to push these changes back into Scala's library.

REQUIREMENTS
============

Build using SBT 10. Will port to SBT 11 soon.


EXAMPLES
========

Basic example of addition:

    import com.azavea.math.Numeric
    import com.azavea.math.FastImplicits._
    
    def adder[A:Numeric](a:A, b:A) = a + b

Creating a Point3 object with coordinates of any numeric type:

    import com.azavea.math.Numeric
    import com.azavea.math.FastImplicits._
    
    case class Point3[T:Numeric](x:T, y:T, z:T) {
      def +(rhs:Point3[T]) = Point(x + rhs.x, y + rhs.y, z + rhs.z)

      def distance(rhs:Point3[T]) = {
        val dx = x - rhs.x
        val dy = y - rhs.y
        val dz = z - rhs.z
        val d = dx * dx + dy * dy + dz * dz
        math.sqrt(d.toDouble)
      }
    }

Mixing literals and variables:

    import com.azavea.math.Numeric
    import com.azavea.math.EasyImplicits._
    import Predef.{any2stringadd => _, _}
    
    def foo[T:Numeric](a:T, b:T):T = a * 100 + b

Currently there are two different ways to use Numeric: EasyImplicits allows you
to operate on mixed numeric types (e.g. T + U + Int). FastImplicits sacrifices
this ability in exchange for speed. Both can be made equally fast (as fast as
operating on direct types) with the optimized-numeric compiler plugin.

If you plan to use the compiler plugin, or aren't worried about speed, you will
probably want to use EasyImplicits.


ANNOYING PREDEF
===============

You may have noticed this ugly-looking import:

    import Predef.{any2stringadd => _, _}

This is to work around a design problem in Scala. You may not always need to
use this, but if you notice problems with + not working correctly you should
add this top-level import. Sorry! :(


PROJECT STRUCTURE
=================

Here is a list of the SBT projects:

  * root:   contains the library code itself ("package" builds the library jar)
  * plugin: contains the compiler plugin code ("package" builds the plugin jar)
  * perf:   contains the performance test

Use "projects" to view them, and "project XYZ" to switch to XYZ.

USING THE PLUGIN
================

The optimized-numeric plugin is able to speed things up by rewring certain
constructions into other, faster ones. Here's an example:

    // written
    def foo[T:Numeric](a:T, b:T) = a + b

    // compiled
    def foo[T](a:T, b:T)(implicit ev:Numeric[T]) = new FastNumericOps(a).+(b)

    // compiled with plugin
    def foo[T](a:T, b:T)(implicit ev:Numeric[T]) = ev.add(a, b)

In the future scalac might be able to do this for us (or hotspot might be able
to optimized it away). But in the absence of these things the plugin helps make
Numeric much faster (especially when using EasyImplicits).

At the most basic level, you can add "-Xplugin:path/to/optimized-numeric.jar"
to your scalac invocation to compile things with the plugin.

When running the perf project, here are the steps you can take to build and
enable the plugin:

  1. in sbt: "project plugin", then "package"
  2. cp plugin/target/scala-2.9.1.final/optimized-numeric-plugin_2.9.1-0.1.jar perf/lib/
  3. uncomment perf/build.sbt line involving -Xplugin
  4. in sbt: "project perf", then "run"

The plugin is enabled by default. To disable the plugin just revert step #3.


BENCHMARKS
==========

To run the benchmarks, do the following:

  1. optionally build and install the plugin
  2. in sbt: "project perf", then "run"

The output shows the speed (in milliseconds) of a direct implementation
(without generics), the new implementation (com.azavea.math.Numeric) and the
old implementation (the built-in scala.math.Numeric). In some cases there is no
old implementation--in those cases the test tries to hem as closely as possible
to the direct implementation.

  * n:d is how new compares to direct
  * o:d is how old compares to direct
  * o:n is how old compares to new

It also creates a benchmark.html file which colors the output.

RESULTS
=======

There are some interesting results:

1. Both scala.math.Numeric and com.azavea.math.Numeric seem to perform worse
on integral types than fractional ones. this is very pronounced for
scala.math.Numeric and only slight for com.azavea.math.Numeric.

2. com.azavea.math.Numeric mostly* performs as well as direct implementations
except when using infix operators without the compiler plugin. The current
Numeric is clearly inappropriate for any application where performance is
important.

3. The asterisk in the previous item has to do with Quicksort. Basically,
scala.util.Sorting uses Ordering\[A\] which is not specialized and which
implements all its own (non-specialized) comparison operators in terms of
compare().

    This ends up being really slow, so my Numeric trait doesn't extend it, but
instead provides a getOrdering() method (which builds a separate Ordering
instance wrapping the Numeric instance). As a result, it doesn't perform any
better than scala.math.Numeric on this test (and in fact does a bit worse).

    I don't know how likely it is that Ordering will be specialized, but huge
performance gains seem possible.

4. scala.util.Sorting.quickSort lacks a direct Long implementation, so using it
with Longs is ~5x slower than Int, Float or Double.

5. It seems like scala.util.Sorting could use some love. My naive direct
implementation of merge sort seems to beat Sorting.quickSort for Long
(obviously), Float and Double. That said, optimizing sort algorithms can be
tricky. But gains seem possible. 


DIFFERENCES
===========

While very similar to scala.math.Numeric, com.azavea.math.Numeric has
some differences. The most significant ones are:

1. It does not inherit from the Ordering type class, but rather directly
implements the comparison methods. I will try to do some cleanup on this and
make it more compatible with Ordering, but it was important to me that the
comparison methods are also specialized.

2. It does not implement Integral/Fractional. I think that leaving
division/modulo off of Numeric is a mistake, and don't think that forcing users
to use Integral/Fractional is a good idea. Given that Scala uses the same
symbol (/) to mean both "integer division" and "true division" it seems clear
that Numeric can too.

3. It's in a different package.

4. It adds some operators that I thought would be nice to have:

  - <=> as an alias for compare
  - === as an alias for equiv
  - !== as an alias for !equiv
  - ** as an alias for math.pow

5. It adds a full-suite of conversions. Unlike the existing Numeric, you can
convert directly to/from any numeric type. This is useful since you might be
going from a generic type to a known type (e.g. A -> Int) or a known type to a
geneirc one (Int -> A). In both cases it is important not to do any unnecessary
work (when A is an Int, you should not have to do any copying/casting).

    These conversions can be used from the instance of Numeric[T] or directly
on the values of T themselves:

        def foo[T:Numeric](t:T):Double = {
            val i:Int = numeric.toInt(t)
            val d:Double = t.toDouble
            d - i
        }

CAVEATS
=======

This section is just for "known problems" with the Numeric type class approach
in Scala.

Precision
---------

Given the signatures of Numeric's functions, it's possible to mix literals and
generic types in a way which will lose precision. Consider:

    def foo[T:Numeric](t:T) = t + 9.2

The author might expect that foo\[Int\](5) will return a Double (14.2), but in
fact foo\[T\] returns T and so Foo\[Int\] will return an Int (14). The solution
in cases like this (if you know you want a double) is to convert T to a Double
first:

     def foo[T:Numeric](t:T) = t.toDouble + 9.2

You could imagine that + could "figure out" whether T has more precision than
Double (e.g. BigDecimal) or less precisoin (e.g. Int) and "do the right thing".
Sadly, this is not possible using the current strategy.

If you are interested in implementing a numeric tower in Scala (presumably by
writing some pretty intense compiler plugins if not modifying Scala's type
system) please get in touch with the author. :)

Clunky syntax
-------------

Unfortunately in order to benefit from the speed of this library you must
annotate all your numeric with @specialization. Also, you often need a
manifest (for instance when you want to allocate Arrays of that type), so you
have to provide those too. Your code will end up looking similar to this:

    def handleInt(a:Int) = ...
    
    def handleA[@specialized A:Numeric:Manifest](a:A) = ...

When you compare these visually the second is obviously terrible. In many of
the examples I have omitted the @specialized annotation and the Manifest type
bound for clarity. Hopefully this is not too deceptive.

It would be great if there was some way to create a type bound that "included"
specialization. For instance:

    // written
    def bar[T:SNumeric](a:T, b:T, c:T) = a * b + c

    // compiled
    def bar[@specialized(Int,Long,Float,Double) T:Numeric](a:T, b:T, c:T) = a * b + c

The current design of specialization works well when a library author expects
users to call her generic functions with concrete types. But if her users are
themselves defining generic functions, the library author is powerless to help.

Obviously, this could pose real problem in terms of a bytecode explosion, which
is why it should not be a default behavior. In fact, Numeric may be one of the
few places where this behavior would be desirable.

Non-extensibility
-----------------

While you should be able to implement your own instances of Numeric for
user-generated types (e.g. Complex or BigRational) you will not be able to use
the full power of this library due to the need to add methods to the
ConvertableFrom and ConvertableTo traits.

This is unfortunate, and I'm is exploring a pluggable numeric conversion
system. However, in the interested of getting a fast, working implementation
out there, I have shelved that for now.

Lack of range support
---------------------

I am still working on implementing a corresponding NumericRange. For now
you'll need to convert to a known type (e.g. Long), or use while loops. This
is definitely possible and will hopefuly be done soon.
