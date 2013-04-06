package j.lang.primitives.cheating

import j.lang.datatypes.JFuncRank
import j.lang.datatypes.JTypeMacros.jINT
import j.lang.datatypes.array.ArrayImplicits.i2j
import j.lang.datatypes.array.JArray
import j.lang.datatypes.array.types.JNumberTypes.JInfinity
import j.lang.datatypes.array.types.JNumberTypes.JInt
import j.lang.datatypes.function.JVerb1Type
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object JCheating {
	val merge = new JVerb1Type[JInt](
	    "merge",
	    List(JFuncRank(1)),
	    jINT
	){
		override def monadImpl[T <: JInt : Manifest](y: JArray[T]) = 
		  throw new NotImplementedException()
		
		override def dyadImpl[T1 <: JInt : Manifest, T2 <: JInt : Manifest](
		    x: JArray[T1], y: JArray[T2]) = {
			
		    var (i,j) = (0,0)
      
		    JArray(jINT, List(x.numItemz + y.numItemz),Vector() ++ (for (walk <- 0 until (x.numItemz + y.numItemz)) yield {
		    	if (x.numItemz.v == i) {
		    	  j += 1
		    	  y.ravel(j-1)
		    	}
		    	else if (y.numItemz.v == j) {
		    	  i += 1
		    	  x.ravel(i-1)
		    	}
		    	else if (x.ravel(i) < y.ravel(j)) {
		    		i += 1
		    		x.ravel(i-1)
		    	}
		    	else {
		    		j += 1
		    		y.ravel(j-1)
		    	}
           }))
		}
	}
	
  final object reverse extends JVerb1Type[JInt](
      "|.",
      List(JFuncRank(JInfinity, 1, JInfinity)),
      jINT
  ){
    protected override def monadImpl[T <: JInt : Manifest](y: JArray[T]) = {
      JArray(y.jaType, y.shape, (0 until y.numItemz).reverse.map(i => {
        y(i)
      }).foldLeft(Vector[T]())(_ ++ _.ravel))
/*      JArray(y.jaType, y.shape,
          (0 until y.numItemz).reverse.map(
              i => y.ravel.slice(i, i*y.itemSize)).foldLeft(
                  Vector[JArrayType]())(_ ++ _))*/
    }
    
    protected override def dyadImpl[T1 <: JInt : Manifest, T2 <: JInt : Manifest](x: JArray[T1], y: JArray[T2]) = {
      throw new NotImplementedException()
    }
  }
}