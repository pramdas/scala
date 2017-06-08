package sqrt

object Sqrt extends App {

	def sqrt (x:Double) = {
		def abs (x : Double) = if (x < 0) -x else x;

		def sqrtIter (guess:Double) : Double =
					if (isGoodEnough(guess)) guess
					else sqrtIter (improve (guess))

		def improve (guess:Double) =
						(guess + x / guess) / 2   
					
		def isGoodEnough (guess : Double) =
						abs(guess * guess - x) / x < 0.001

		sqrtIter (1.0)

	}

	println ("Testing Square Root");
	println ("Square Root of 100 " + sqrt(100));
	println ("Square Root of 225 " + sqrt(225));
}
