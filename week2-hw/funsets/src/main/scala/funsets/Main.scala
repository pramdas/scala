package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val s2 = singletonSet (2)
  val s3 = singletonSet (3)
  val s4 = singletonSet (4)
  val s5 = singletonSet (5)
  val s6 = singletonSet (6)
  val s7 = singletonSet (7)
  val s8 = singletonSet (8)



  val u1 = union (s2, s4)
  val u2 = union (s4, s5)
  val uEven = union(union(union(s2,s4),s6),s8)
  val uEvenOdd = union(union(union(union (s2,s4),s6),s7),s8)

  println ("u1 is 2 and 4")
  println ("u2 is 4 and 5")

  println ("Contains 4 in  Union (2,4) =  " + contains (u1, 4))
  println ("Contains 3 in  Union (2,4) =  " + contains (u1, 3))


  println ("Does 4 intersect u1 and u2 = " + contains (intersect (u1, u2), 4))
  println ("Does 5 intersect u1 and u2 = " + contains (intersect (u1, u2), 5))
	

  println ("Does 4 diff u1 and u2 = " + contains (diff (u1, u2), 4))
  println ("Does 2 diff u1 and u2 = " + contains (diff (u1, u2), 2))
  println ("Is 4 in u2 and even = "  + filter (u2, x => (x % 2) == 0)(4))
  println ("Is 5 in u2 and even = "  + filter (u2, x => (x % 2) == 0)(5))


  println ("Are all elements in uEven even = " + forall(uEven, x=>((x % 2)==0)))
  println ("Are all elements in uEvenOdd even = " + forall(uEvenOdd, x=>((x % 2)==0)))


  println ("exists in all elements in uEven even = " + exists(uEven, x=>((x % 2)==0)))
  println ("exists in all elements in uEvenOdd even = " + exists(uEvenOdd, x=>((x % 2)==0)))
  println ("exists in all elements in uEvenOdd odd = " + exists(uEvenOdd, x=>((x % 2)==1)))
}
