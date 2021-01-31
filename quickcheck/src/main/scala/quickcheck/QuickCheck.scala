package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.Nil


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val m = math.min(a, b)
    val h = insert(b, insert(a, empty))
    findMin(h) == m
  }

  property("empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("sort") = forAll { (h: H) =>
    def mkList(h: H): List[Int] =
      if (isEmpty(h)) List()
      else findMin(h) :: mkList(deleteMin(h))

    mkList(h) == mkList(h).sorted
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val m = meld(h1, h2)
    val minMeld = findMin(m)
    minMeld == min1 || minMeld == min2
  }

}
