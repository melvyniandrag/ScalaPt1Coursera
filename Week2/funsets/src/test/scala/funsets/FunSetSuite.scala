package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def filter_fun_1(x : Int): Boolean = {
      x == 1
     }
     def less_than_2000(x : Int): Boolean = {
       x < 2000
     }
     def test_exist_fun(x : Int) : Boolean = {
       x == 2
     }
      val s1_2 = union(s1, s2)
      val s1_2_3 = union(s1_2, s3)
  }
  
  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("filter is working properly"){
   new TestSets {
     val f = filter(s1, filter_fun_1)
     assert(contains(f, 1))
   }
  }
  
  test("forall works properly"){
    new TestSets {
      assert(forall(s1_2_3, less_than_2000))
    }
  }
  
  test("exists works properly"){
    new TestSets {
      assert(exists(s1_2_3, test_exist_fun))
    }
  }
  
  test("map works correctly"){
    new TestSets {
      val f = map(s1_2_3, x=>x*x)
      assert(contains(f, 1))
      assert(contains(f, 4))
      assert(contains(f, 9))
    }
  }
 }