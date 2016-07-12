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
		val charListA = List('a', 'a', 'a')
		val charListAB = charListA ::: List('b', 'a', 'b')
		val charListABC = charListAB ::: List('c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')
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

  test("times of a tree olist of as."){
    new TestTrees{
      assert(times(charListA) == List(('a', 3)))
    }
  }
  /*
  test("times of a list of as and bs."){
  	// This works alright, but I think there is more to comparing lists than just saying '=='
    new TestTrees{
      assert(times(charListAB) == List(('a', 4), ('b', 2))
    }
  }
  */
  /*
  test("ordered list freqlistA"){
    // Works, but as above I don't know how to compare lists yet. I guess that is why there aren't any tests for these routines.
    new TestTrees{
      assert(makeOrderedLeafList(times(charListA)) == List(('a', 3)))
    }
  }
  */
  /*
  test("ordered list freqlistABC"){
    // Works. List comparison not implemented yet.
    new TestTrees{
      assert(makeOrderedLeafList(times(charListABC)) == List(('c', 10), ('a', 3), ('b', '2')))
    }
  }
  */
  
  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test ("singleton test for different lists of trees"){
    assert(singleton(Nil) == false)
    assert(singleton(List(Leaf('e', 2))) == true)
    assert(singleton(List(Leaf('e', 2), Leaf('a', 5))) == false)
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("test until on leaflist"){
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'),7)))
  }

  test("decode test"){
    assert(decodedSecret === "huffmanestcool".toList)
  }
  
  test("make tree, encode, decode text"){
    val huffmanTree = createCodeTree("thequickbrownfoxjumpsoverthelazydog".toList)
    val testString = "iloveyou".toList
    assert(decode(huffmanTree, encode(huffmanTree)(testString)) == testString)
  }
  
  test("decode and encode a short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abba".toList)) === "abba".toList)
    }
  }
  test("test convert function"){
    new TestTrees{
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

}
