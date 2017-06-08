package FixedPoint

import math.abs

object FixedPoint extends App {
	val tolerance = 0.0001

	def isCloseEnough (x:Double, y:Double) =
		abs ((x-y)/x) < tolerance

	def fixedPoint (f:Double=>Double)(firstGuess:Double) = {
		def iterate (guess : Double) : Double = {
			val next = f(guess);
			println ("Guess is " + guess + " Value is " + next)

			if (isCloseEnough (guess, next)) next else 
				iterate (next)
		}


		iterate (firstGuess)
	}

	def averageDamp(f:Double => Double)(x:Double) = (x+f(x))/2

	def squareRoot (x:Double) = fixedPoint(averageDamp(y=>x/y))(1.0)

	def sqrt (x : Double) = fixedPoint (y => (y+x/y)/2)(1.0)

	//sqrt (2);

	squareRoot (4);





}


