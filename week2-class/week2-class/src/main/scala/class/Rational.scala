package Rational

object Rational extends App {
	val x = new Rational (2,3)
	val y = new Rational (3,4)

	println (x + y).toString;

	println (y + y).toString;
}

class Rational (x:Int , y:Int) {
	require (y != 0, "denominator must be positive");

	def this (x:Int) = this (x, 1)


	private def gcd (a:Int, b:Int):Int = if (b==0) a else gcd (b, a % b)
	def numer = x
	def denom = y

	def < (that:Rational) = numer * that.denom < that.numer * denom

	def max (that:Rational) = if (this < that) that else this

	def + (that: Rational) = 
		new Rational (
			numer * that.denom + that.numer * denom,
			denom * that.denom)

	def unary_- : Rational = new Rational (-numer, denom)

	def sub (that : Rational) = this + -that;

	override def toString = {
		val g = gcd (this.numer, this.denom)
		numer / g + "/" + denom / g
	}

}
