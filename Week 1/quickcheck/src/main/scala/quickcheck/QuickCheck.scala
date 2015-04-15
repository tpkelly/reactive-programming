package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min") = forAll { (a: Int, b: Int, c: Int) =>
  	val sorted = List(a, b, c).sorted
    
    val x = insert(sorted(0), empty)
    val y = insert(sorted(1), x)
    val z = insert(sorted(2), y)
    
    findMin(z) == sorted(0)
  }
  
 property("delete") = forAll { (a: Int, b: Int, c: Int) =>
  	val sorted = List(a, b, c).sorted
    
    val x = insert(sorted(0), empty)
    val y = insert(sorted(1), x)
    val z = insert(sorted(2), y)
    val w = deleteMin(z)
    
    findMin(w) == sorted(1)
  }
  
  property("meld") = forAll { (a: Int, b: Int) =>
    val min = Math.min(a, b)
    val max = Math.max(a, b)
    
    val x = insert(min, empty)
    val y = insert(max, empty)
    val z = meld(y, x)
    
    findMin(z) == min
  }
  
  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
