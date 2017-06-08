package pattern

trait Expr
case class Number (n:Int) extends Expr
case class Sum (e1:Expr, e2:Expr) extends Expr
case class Var (x:Char) extends Expr
case class Prod (e1:Expr, e2:Expr) extends Expr


object exprs extends App {
	def show (e: Expr) : String = e match {
		case Number (x) => x.toString
		case Sum (l,r) => show (l) + " + " + show (r)
		case Var (x) => x.toString
		case Prod (e1, e2) => 
			val str1 = e1 match {
				case Sum (a,b) => "(" + show (Sum(a,b)) + ")"
				case _ => show (e1)
			}
			val str2 = e2 match {
				case Sum (a,b) => "(" + show (Sum(a,b)) + ")"
				case _ => show (e1)
			}
			str1 + "*" + str2
	}

	println(show (Sum (Number(1), Number(44))))
	println(show(Prod(Sum(Number(2),Var('x')),Var('y'))))
}
