package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")

	//println (pascal (2,4));
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }


	val testBalance : List[String] = List (
			"())(",
			"(if (zero? x) max (/ 1 x))",
			":-)",
			"I told him (that it’s not (yet) done). (But he wasn’t listening)"
		)


	for (testStr <- testBalance) {
		println ("Call to Balance with " + testStr + " returns " + balance(testStr.toList))
	}

	println ()
	val listCoins : List[Int] = List (1,2);
	println ("Count Change for 3 " + countChange (3, List(1,2)) + " ways");
	println ("Count Change for 4 " + countChange (4, List(1,2)) + " ways");
	println ("Count Change for 5 " + countChange (5, List(1,2)) + " ways");
	println ("Count Change for 6 " + countChange (6, List(1,2,3)) + " ways");



  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
		if (c==0 || (c==r)) return 1;

		pascal (c-1, r-1) + pascal (c, r-1);

	}
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

		def loopBalance (balancePara : Int, chars : List[Char]) : Int = {
			if (chars.isEmpty) balancePara
			else {
				var count = balancePara;	
				var first : Char = chars.head 
				if (first == '(' && balancePara >= 0 )   count+=1
				else if (first == ')') count-=1

				loopBalance (count, chars.tail)
			}
		}

		var finalBalance = loopBalance (0, chars)

		if (finalBalance == 0 ) 
			true
		else
			false
	}
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

		def buildChangeList (remainder : Int, denominations : List[Int], count : Int) : Int = {
			/*
			println ("Build Change List called with remainder = " + remainder + " list = " + 
							denominations.foreach (println) );
			*/
			var updatedCount = count;
			if (remainder == 0) {
				updatedCount += 1; 
				//println ("Updating combinations " + updatedCount); 
				return updatedCount;
			}
			else if (denominations.isEmpty) {return updatedCount}
			else if (denominations.head > remainder ) {return updatedCount}

			var updatedCount1 = buildChangeList (remainder-denominations.head, denominations, count)
			var updatedCount2 = buildChangeList (remainder, denominations.tail, updatedCount1)

			return updatedCount2;
		}

		return buildChangeList (money, coins.sortWith (_ < _), 0)


  }
}
