package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { h: H =>
    val m = realMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == math.min(a, b)
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("deleteMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    val hh = deleteMin(h)
    isEmpty(hh)
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll { h: H =>
    val l = genList(h)
    isSorted(l)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("minimum of two") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) realMin(m) == 0
    else if (!isEmpty(h1) && isEmpty(h2)) findMin(m) == findMin(h1)
    else if (isEmpty(h1) && !isEmpty(h2)) findMin(m) == findMin(h2)
    else if (findMin(h1) <= findMin(h2)) findMin(m) == findMin(h1)
    else findMin(m) == findMin(h2)
  }

  property("meld sorted") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    val l = genList(m)
    isSorted(l)
  }

  property("deleteMin2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    deleteMin(h2) == insert(math.max(a, b), empty)
  }

  property("deleteMin3") = forAll { l: List[Int] =>
    val ll = l.sorted
    val h = ll.foldLeft(empty)((accu, curr) => insert(curr, accu))
    genList(h) == ll
  }

  property("deleteMin4") = forAll { l: List[Int] =>
    if (l.size == 0) true
    else {
      val ll = l.sorted
      val h = ll.foldLeft(empty)((accu, curr) => insert(curr, accu))
      ll.tail == genList(deleteMin(h))
    }
  }

  def realMin(h: H): Int = if (isEmpty(h)) 0 else findMin(h)

  def genList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: genList(deleteMin(h))
  }

  def isSorted(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case x :: xss => if (xss.isEmpty) true else (x <= xss.head) && isSorted(xss)
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
