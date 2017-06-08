package example


object Hello extends Greeting with App {
	def removeAt[T] (n:Int, xs: List[T]) : List[T] = {
	   def iterateToRemove (index: Int, xs: List[T], rs: List[T]) : List[T] = {
		   if (index == n) rs ::: xs.tail
		   else iterateToRemove (index+1, xs.tail, rs ::: List(xs.head))
		}

		iterateToRemove (0, xs, List())
	}

	println(greeting)

	println (removeAt (1, List ('a', 'b', 'c', 'd')))
	println (removeAt (2, List ('a', 'b', 'c', 'd')))
}

trait Greeting {
  lazy val greeting: String = "hello"
}
