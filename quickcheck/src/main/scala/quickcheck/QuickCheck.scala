package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import annotation.tailrec
import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  @tailrec
  private def gt(min: Int, heap: H): Boolean =
    if (isEmpty(heap)) true
    else {
      val foundedMin = findMin(heap)
      if (foundedMin >= min) gt(foundedMin, deleteMin(heap))
      else false
    }

  private def clearAll(heap: H): List[Int] =
    if (isEmpty(heap)) List.empty
    else findMin(heap) :: clearAll(deleteMin(heap))


  property("Add first then empty. It shpuld empty") =
    forAll {
      number: Int => isEmpty(deleteMin(insert(number, empty)))
    }

  property("Add minimum element, it must be itself") =
    forAll {
      number: Int => number == findMin(insert(number, empty))
    }

  property("Delete and insert min") =
    forAll {
      (heap: H) =>
        val min = if (isEmpty(heap)) 0 else findMin(heap)
        min == findMin(insert(min, heap))
    }

  property("Insert element after delete it") =
    forAll {
      (list: List[Int]) => list.sorted == clearAll(list.foldLeft(empty) {
        (heap, number) => insert(number, heap)
      })
    }



  property("Find min melded heap") =
    forAll {
      (heap1: H, heap2: H) =>
        (for {
          m3 <- Try {
            findMin(meld(heap1, heap2))
          }
          a <- Try {
            m3 == findMin(heap1)
          }
          b <- Try {
            m3 == findMin(heap2)
          }
        } yield a || b) getOrElse true
    }


  property("Delete min element > Integer.MINVALUE") =
    forAll {
      (heap: H) => gt(Integer.MIN_VALUE, heap)
    }


  lazy val genHeap: Gen[H] = frequency(
    (1, empty),
    (10, for {
      a <- arbitrary[A]
      h <- genHeap
    } yield {
      insert(a, h)
    }))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
