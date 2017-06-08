package example

object Hello extends Greeting with App {
  println(greeting)

  val t1 = new NonEmpty (3, Empty, Empty)
  val t2 = t1 incl 4
  val t3 = t2 incl 5
  val t6 = t3 incl 2
  //val t4 = new NonEmpty (2, Empty, Empty)
  //val t5 = t3 union t4


  println ("Tree structure at root 3 " + t1.toString)
  println ("Tree structure with child 4 " + t2.toString)
  println ("Tree structure with child 5 " + t3.toString)
  println ("Tree structure with child 2 " + t6.toString)
}

abstract class IntSet {
	def incl(x:Int): IntSet
	def contains (x:Int):Boolean
	def union (other:IntSet) : IntSet
}

object Empty extends IntSet {
	def contains (x: Int) : Boolean = false
	def incl (x : Int) : IntSet = new NonEmpty (x, Empty, Empty)
	override def toString = "."
	def union (other:IntSet) : IntSet = other
}

class NonEmpty (elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains (x:Int) : Boolean = {
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
	}

	def incl (x:Int) : IntSet = {
		if (x < elem) new NonEmpty (elem, left incl x, right)
		else if (x > elem) new NonEmpty (elem, left, right incl x)
		else this
	}

	def union (other : IntSet) : IntSet = 
		((left union right) union other) incl elem

	override def toString = "{" + left + elem + right + "}"


}



trait Greeting {
  lazy val greeting: String = "hello : Testing class hierarchies"
}
