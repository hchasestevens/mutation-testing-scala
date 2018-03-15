package scalatest

import org.scalatest.FunSpec

class UniquerSpec extends FunSpec {
  val uniquer = new unique.Uniquer
  val list = List(2, 1, 2, 1, 3)
  val expectedList = List(1, 2, 3)

  describe("uniqueInts") {

//    it ("does not raise runtime exceptions") {
//      uniquer.uniqueInts(list)
//    }
////
//    it ("contains same or fewer items") {
//      assert(uniquer.uniqueInts(list).length <= list.length)
//    }
////
//    it ("contains all original items") {
//      val uniqued = uniquer.uniqueInts(list)
//      assert(list.forall(n => uniqued.contains(n)))
//    }
//
    it ("contains all original items only once") {
      val uniqued = uniquer.uniqueInts(list)
      val emptyMap: Map[Int, Int] = Map()
      val valueCounts = uniqued.foldLeft(emptyMap) {
        (counts, key) => {
          val curCount = counts getOrElse (key, 0)
          counts + (key -> curCount.+(1))
        }
      }
      val originalItemCounts = for {
        originalItem <- list
      } yield valueCounts getOrElse (originalItem, 0)
      assert(originalItemCounts.forall(v => v == 1))
    }

  }
}
