package example

object Hello extends Greeting with App {
  println(greeting)
  println ("sum of sqares of numbers between 1, 3 =  " + sum (x=>x*x,1,3))

  println ("Product of numbers from 1 to 4 =  "  + product (x=>x)(1,4))
  println ("Factorial of numbers 5 =  "  + factorial(5))
  println ("Product of numbers from 1 to 4 as mapReduce =  "  + 
	  genericOpFunc (x=>x, 1, (a,b) => a*b)(1,4))

	def sum (f:Int => Int, a:Int, b:Int) : Int = {
		def loop (a:Int, acc:Int) : Int = {
			if (a>b) acc 
			else loop (a+1, f(a) + acc); 
		}

		loop (a, 0)

	}

	def product(f:Int => Int)(a : Int, b : Int) : Int = {
		if(a > b) 1 else f(a) *  product (f)(a+1, b)
	}

	def factorial (a : Int) : Int = {
		if (a < 0 ) 1 else product (x=>x)(1,a)
	}


	def genericOpFunc (f:Int=>Int, unit : Int, combine:(Int, Int) => Int)(a:Int, b:Int) : Int = {
		if (a>b) unit else
		combine (f(a), genericOpFunc(f, unit, combine)(a+1, b))
	}


}

trait Greeting {
  lazy val greeting: String = "hello week 2"
}

