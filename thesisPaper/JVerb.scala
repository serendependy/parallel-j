package j.lang.datatypes.function

/*Imports*/

abstract class JVerb[M <: JArrayType : Manifest, 
  D1 <: JArrayType : Manifest, D2 <: JArrayType : Manifest,
  MR <: JArrayType : Manifest, DR <: JArrayType : Manifest]
  (rep: String, val ranks: List[JFuncRank], 
  mdomain: JTypeMacro, 
  d1domain: JTypeMacro, d2domain: JTypeMacro) extends 
  JFunc[JArray[M], JArray[D1], JArray[D2], 
        JArray[MR], JArray[DR]](rep, jVERB, 
		mdomain, d1domain, d2domain) {

  def apply[T <: JArray[M]](y: T) = monad(y)
  
  override def monad[T <: JArray[M]](y: T) = { 
	  val jaf = JArrayFrame(ranks.map(_ r1), y)

	   val newCells = if (!JVerb.parallelFlag) {
	    (0 until jaf.frameSize) map { fr =>
	    monadImpl(JArray(jaf.jar.jaType, jaf.cellShape, 
			jaf.jar.ravel.slice(fr*jaf.cellSize, 
				(1+fr)*jaf.cellSize)))
	  }}
	   else {
   	    (0 until jaf.frameSize).par map { fr =>
	    monadImpl(JArray(jaf.jar.jaType, jaf.cellShape, 
			jaf.jar.ravel.slice(fr*jaf.cellSize, 
				(1+fr)*jaf.cellSize)))
	   }}
	  
	  val newShape = jaf.frames.dropRight(ranks.length
	  	).foldLeft(List[Int]())(_ ++ _) ++ newCells(0).shape
	  JArray(newCells(0).jaType, newShape, 
	  	newCells.foldLeft(Vector[MR]())(_ ++ _.ravel))
	}
	
  	def apply[T1 <: JArray[D1], T2 <: JArray[D2]](x: T1, y: T2) = dyad(x,y)
  	
	override def dyad[T1 <: JArray[D1], T2 <: JArray[D2]](x: T1, y: T2) = {
	  val jafx = JArrayFrame(ranks.map(_ r2), x)
	  val jafy = JArrayFrame(ranks.map(_ r3), y)

	  jafx.shapeAgreement(jafy) match {
	    case None => throw new Exception()
	    case Some(agree) => {
	      val xreframed = jafx.shapeToNewFrame(agree)
	      val yreframed = jafy.shapeToNewFrame(agree)

	      val xcellShape = jafx.frames.last
	      val xcellSize = xcellShape.foldLeft(1)(_ * _)
	      val ycellShape = jafy.frames.last
	      val ycellSize  = ycellShape.foldLeft(1)(_ * _)
	      val frameSize  = agree.init.foldLeft(1)(_ * _.foldLeft(1)(_ * _))
	      
	      val newCells = if (!JVerb.parallelFlag) { 
	        (0 until frameSize) map { fr =>
	        dyadImpl(
				JArray(jafx.jar.jaType, xcellShape,
					xreframed.ravel.slice(fr*xcellSize, 
						(1+fr)*xcellSize)),
	        	 JArray(jafy.jar.jaType, ycellShape, 
					yreframed.ravel.slice(fr*ycellSize, 
						(1+fr)*ycellSize)) )
	      }}
	      else {
	        (0 until frameSize).par map { fr =>
	        dyadImpl(
				JArray(jafx.jar.jaType, xcellShape, 
					xreframed.ravel.slice(fr*xcellSize, 
						(1+fr)*xcellSize)),
	        	JArray(jafy.jar.jaType, ycellShape, 
				 	yreframed.ravel.slice(fr*ycellSize, 
						(1+fr)*ycellSize)) )	        
	      }}

	      val newShape = agree.dropRight(1).foldLeft(
		  	List[Int]())(_ ++ _) ++ newCells(0).shape
	      JArray(newCells(0).jaType, newShape, 
		  	newCells.foldLeft(Vector[DR]())(_ ++ _.ravel))
	    }
	  }
	}
  
 def addRanks(r: JFuncRank) = {
    val outerRef = this
    new JVerb[M,D1,D2,MR,DR](
        rep + "(\"" + r + ")",
        ranks :+ r,
        mdomain, d1domain, d2domain) {
     
      override def monadImpl[T <: M : Manifest](y: JArray[T]) = {
        outerRef(y)
      }
      override def dyadImpl[T1 <: D1 : Manifest, 
	  	T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]) = 
        outerRef(x, y)
    }
  }

	protected def monadImpl[T <: M : Manifest](y: JArray[T]): JArray[MR]
	protected def dyadImpl[T1 <: D1 : Manifest, 
		T2 <: D2 : Manifest](x: JArray[T1], y: JArray[T2]): JArray[DR]
}

object JVerb {
  var parallelFlag = false
}
