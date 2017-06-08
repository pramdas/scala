package Poly

object Poly extends App {
	def singleton[T] (elem:T) = new Cons[T](elem, new Nil[T])
	singleton[Int](1)
	singleton[Boolean](true)

	val intList : List[Int] = new Cons (1, new Cons(2, new Cons (3, new Nil[Int])))

	val findElement : Int = findListIndex (2, intList)

	println ("Element Found " + findElement)

	def findListIndex[T] (index:Int, list : List[T]) : T = {
		def iterateListToIndex (count:Int, iterateList : List[T]) : T = {
			if (iterateList.isEmpty) 
				throw new IndexOutOfBoundsException ("Bounds crossed at " + count)
			else if (count == index) iterateList.head 
			else iterateListToIndex (count+1, iterateList.tail)
		}

		iterateListToIndex (0, list)
	}
}

trait List[T] {
	def isEmpty : Boolean
	def head : T
	def tail : List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
}

class Nil[T] extends List[T] {
	def isEmpty : Boolean = true
	def head : Nothing = throw new NoSuchElementException ("Nil.head")
	def tail : Nothing = throw new NoSuchElementException ("Nil.tail")

}

object List {
	def  apply[T] () : List[T] = {
		new Nil[T]
	}

	def apply[T] (a:T, b:T) : List[T] = {
		var twoList = new Cons (a, new Cons (b, new Nil [T]))
		twoList
	}

	def apply[T] (a:T, b:T, c:T) = {
		var threeList = new Cons (a, new Cons (b, new Cons (c, new Nil[T])))
		threeList
	}

}


