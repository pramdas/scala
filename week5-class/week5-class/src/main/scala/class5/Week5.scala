package week5
import math.Ordering

object week5Class extends App {
	def msort (xs:List[Int]) : List[Int] = {
		val n  = xs.length / 2
		if (n == 0) xs
		else {
			def merge (xs : List[Int] , ys : List[Int]) : List[Int] = (xs, ys) match {
				case (Nil,ys) => ys
				case (xs,Nil) => xs
				case (x::xs1,y::ys1) => if (x<y) x::merge(xs1,ys) 
										else y::merge(xs,ys1)
			}
			val (fst, snd) = xs.splitAt (n)
			merge (msort(fst), msort(snd))
		}
	}

	println (msort (List (2,-4,1,6)))

	def msortT[T] (xs:List[T])(implicit ord:Ordering[T]) : List[T] = {
		val n  = xs.length / 2
		if (n == 0) xs
		else {
			def mergeT (xs : List[T] , ys : List[T]) : List[T] = (xs, ys) match {
				case (Nil,ys) => ys
				case (xs,Nil) => xs
				case (x::xs1,y::ys1) =>  if(ord.lt(x,y)) x::mergeT(xs1,ys) 
										else y::mergeT(xs,ys1)
			}
			val (fst, snd) = xs.splitAt (n)
			mergeT (msortT(fst), msortT(snd))
		}
	}

	def squareLists (xs: List[Int]) : List [Int] = xs match {
		case Nil => Nil
		case y::ys => y*y :: squareLists (ys)
	}

	def squareList (xs:List[Int]) : List[Int] = 
		xs map (y=>y*y)

	println (msortT (List (2,-4,1,6))(Ordering.Int))

	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x::xs1 => {
			val (list1,list2) = xs.span (_==x)
			list1 :: pack(list2)
		}
	}

	def encode[T] (xs: List[T]) : List[(T,Int)] = {
		val packList = pack (xs)
		def encodeRec (packL : List[List[T]]) : List[(T,Int)] = packL match {
			case Nil => Nil
			case x::xs => (x.head, x.length) :: encodeRec (xs)
		}

		encodeRec (packList)
	}

	def mapFun[T,U](xs:List[T], f: T=>U) : List[U] = 
		(xs foldRight List[U]()) ((a,b) => f(a) :: b)

	def lengthFunc[T](xs: List[T]) : Int = 
		(xs foldRight 0)((a,b) => 1+b)


	println ("Pack Test pack(List(a, a, a, b, c, c, a)) = " + pack(List("a", "a", "a", "b", "c", "c", "a")))
	println ("Encode Test pack(List(a, a, a, b, c, c, a)) = " + 
		encode(List("a", "a", "a", "b", "c", "c", "a")))

	val listTest = List (1,2,3,4)
	println ("Map Function as squares " + mapFun(listTest, (x:Int)=>x*x))
	println ("Length Function on list " + lengthFunc(listTest))
}
