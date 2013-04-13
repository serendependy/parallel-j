package j.test

/*Imports*/
    
    def testMergeSort(y: JArray[JInt]) {
      println("\n--Testing MergeSort")
      
      val jtwo = JArray.scalar(JInt(2))
      val j16  = JArray.scalar(JInt(16))
      
      val intReverse = reverseShift.asInstanceOf[
	  	JVerb[JInt, JInt, JInt, JInt, JInt]]
      val sort2 = (decrementLesserthanequal insert) agenda(
          reverse,
          rightIdentity.asInstanceOf[JVerb1Type[JInt]])
       
      val divide = (y: JArray[JInt]) => {
        shapeReshape(
        	tallyCopies(
        		naturalLog(
        			jtwo,
        			tallyCopies(y)).asInstanceOf[
						JArray[JFloat]].toJInt,
        		jtwo).asInstanceOf[JArray[JInt]],
        	y).asInstanceOf[JArray[JInt]]
      }
      
      val dim = (y: JArray[JInt]) => {
        tallyCopies(shapeReshape(y))
      }
      
      val sortBase = (sort2 addRanks(JFuncRank(1)) )
      val merger2 = (merge insert) addRanks(JFuncRank(2))
      
      val repeatedMerge = (y: JArray[JInt]) => {
        merger2.power(decrementLesserthanequal(dim(y)
            ).asInstanceOf[JArray[JInt]]).apply(y)
      }
      
      def mergeSort = (y: JArray[JInt]) => {
        repeatedMerge(sortBase(divide(y)))
      }

      println("sorting: " + y) 
      println("sorted:  " + repeatedMerge(sortBase(divide(y))))
       
       println("DONE")
    }
  }
