package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    queue <- frequency(80 -> genHeap, 10 -> empty)
  } yield insert(element, queue)

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
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("deleteSingleElement") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { (h: H) =>
    def checkOrder(h: H, predecessor: Int): Boolean = isEmpty(h) match {
      case true => true
      case false => {
        val min = findMin(h)
        if (predecessor <= min) checkOrder(deleteMin(h), min)
        else false
      }
    }

    checkOrder(h, Int.MinValue)
  }

  property("deleteSingleElement") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("meldAndMin") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

  property("associative meld") = forAll { (h: H, i: H, j: H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    toList(a) == toList(b)
  }

  property("order of mins") = forAll { (h: H) =>
    toList(h).zip(toList(h).drop(1)).forall {
      case (x, y) => x <= y
    }
  }

  def toList(h: H): List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))
}
