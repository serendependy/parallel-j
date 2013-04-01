/*Imports here*/

// TODO this is the wrong method!

def testNumericalIntegration(numRiemann: JArray[JInt]) {
   val recip =  reciprocalDivide(numRiemann)
   val xvals =  signumMultiply(integersIndex(numRiemann),
                               recip)
   val yvals = squarerootRoot(negateMinus(JArray.scalar(JReal.One),
                              squareNotand(xvals)))
   val pi = signumMultiply(
      signumMultiply(
	     (conjugatePlus insert).apply(yvals),
	      JArray.scalar[JInt,Int](4)),
      recip)

   println("PI is: " + pi)
}
