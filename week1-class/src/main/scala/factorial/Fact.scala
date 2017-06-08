package factorial

object Factorial extends App {

	def factorial (n : Int) : Int = {
		def loop (acc : Int, n : Int) : Int = {
			if (n == 0) acc
			else loop (acc * n, n - 1)
		}

		loop (1, n)
	}

	println ("Factorial of 4 is " + factorial (4))
	println ("Factorial of 10 is " + factorial (10))

}
