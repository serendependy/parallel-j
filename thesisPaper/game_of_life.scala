package j.test

/*Imports*/
    
def testGameOfLife(x: JInt, y: JInt, ratioAliveDead: JFloat, steps: Int) = {
	println("\n--Testing Game of Life")
    val boardShape = JArray.vec2(x, y)
    val numCells = (signumMultiply insert).monad(
		boardShape).asInstanceOf[JArray[JInt]]

    val lifeThreshold = signumMultiply(
		JArray.scalar(ratioAliveDead),
    	numCells).asInstanceOf[JArray[JReal]]

    val board = shapeReshape(
          boardShape,
    	  incrementGreaterthanequal(
    	      lifeThreshold,
    	      rollDeal[JArray[JInt],JArray[JInt]](
    	          numCells,
    	          numCells))).asInstanceOf[JArray[JInt]]
      
    val shiftBy = shapeReshape(
          JArray.vec2(8, 2),//    UL	  U		 UR
          JArray.auto[JInt, Int](-1,-1,  -1,0,  -1,1,  
		 // L      R     
		  	0,-1,  0,1,  
		 // DL     D     DR	
			1,-1,  1,0,  1,1)).asInstanceOf[JArray[JInt]]
      
    val neighborArray = (y: JArray[JInt]) => 
		reverseShift(shiftBy, y).asInstanceOf[JArray[JInt]]
    val listNeighbors = (y: JArray[JInt]) => 
		(conjugatePlus insert).monad(
			neighborArray(y)).asInstanceOf[JArray[JInt]]

    val nextState = 
		leftIdentity.asInstanceOf[
			JVerb[JInt, JInt, JInt, JInt, JInt]] agenda(
          new JVerb1Type[JInt](
              "(3 = ])",
              List(JFuncRank(0)),
              jINT){
            override def monadImpl[T <: JArrayType : Manifest](y: JArray[T]) = 
				throw new NotImplementedException()
            
            override def dyadImpl[T1 <: JArrayType : Manifest, 
				T2 <: JArrayType : Manifest](
				x: JArray[T1], y: JArray[T2]) = {

            	equal(JArray.scalar[JInt,Int](3),
            	      y)
            }
          },
      new JVerb1Type[JInt](
          "([: +./ 2 3 = ])",
          List(JFuncRank(0)),
          jINT){
            override def monadImpl[T <: JInt : Manifest](y: JArray[T]) = 
				throw new NotImplementedException()
            
            override def dyadImpl[T1 <: JInt : Manifest,
				T2 <: JArrayType : Manifest](
				x: JArray[T1], y: JArray[T2]) = {
              (realOr insert).apply(equal(
       		  				JArray.vec2(2,3),
       		  				y)).asInstanceOf[JArray[JInt]]
            }            
          })
 
 	var boardvar = board
      
      for (i <- 0 until steps) {
        boardvar = nextState(boardvar, 
			listNeighbors(boardvar)
			).asInstanceOf[JArray[JInt]]
      }
     
	  return boardvar
    }
