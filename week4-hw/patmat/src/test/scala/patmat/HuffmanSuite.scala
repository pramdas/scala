package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times in chars of a larger tree") {
    new TestTrees {
		val chList = List ('a','a','b','b','c','d', 'a', 'c', 'e', 'f')
		val timeList : List [(Char,Int)] = times (chList) 
		println ("Times List " + timeList.toString)
      //assert(chars(t2) === List('a','b','d'))
		//val orderedLeafList : List [Leaf] = makeOrderedLeafList (timeList)
    }

  }



  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("create code trees") {
	val listChar : List[Char] = string2Chars("helloworld")
    assert(listChar === List('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd'))
	val huffCodeTree = createCodeTree (listChar)

	println ("Huffman CodeTre = " + printCodeTree (huffCodeTree))
  }

  test("frech string decode") {
	println ("French Huffman secret = " + decodedSecret.toString )
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
