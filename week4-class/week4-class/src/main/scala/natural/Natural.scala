package natural

abstract class Nat {
	def isZero : Boolean
	def predecessor : Nat
	def successor : Nat = new Succ (this)
	def + (that:Nat) : Nat
	def - (that:Nat) : Nat
}

object Zero extends Nat {
	def isZero : Boolean = true

	def predecessor : Nat = throw new NoSuchMethodException


	def + (that : Nat) : Nat = that

	def - (that : Nat) : Nat = {
		if (that.isZero) this 
		else throw new Error ("negative number")
	}
}

class Succ(n:Nat) extends Nat {
	def isZero : Boolean = false

	def predecessor : Nat = {
		n
	}


	def + (that:Nat) : Nat = {
		new Succ (n + that)
	}

	def - (that:Nat) : Nat = {
		if (that.isZero) this 
		else n - that.predecessor
		/*
		def loopForMinus (currPredecessor : Nat, decrLeft : Nat) : Nat  = {
			if (decrLeft.isZero == true) currPredecessor
			else loopForMinus (currPredecessor.predecessor, decrLeft.predecessor)
		}

		loopForMinus (this.predecessor, that.predecessor)
		*/
	}
}
