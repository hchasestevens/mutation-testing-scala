package unique

class Uniquer {
  def uniqueGeneric[T](list: List[T]): List[T] =
    list.foldLeft(List(): List[T]) { (out: List[T], item: T) =>
      if (out.contains(item)) {
        out
      } else {
        out :+ item
      }
    }

  def highWaterMarks(list: List[Int]): List[Int] = {
    list.headOption match {
      case Some(head) =>
        val initialState = (head, List(head))
        val finalState = list.tail.foldLeft(initialState) {
          (currentState, newValue) => {
            val highWaterMark = currentState._1
            if (newValue <= highWaterMark) {
              currentState
            } else {
              val currentList = currentState._2
              (newValue, currentList :+ newValue)
            }
          }
        }
        finalState._2
      case _ => list
    }
  }

  def uniqueInts(list: List[Int]): List[Int] = {
    highWaterMarks(list.sorted)
  }
}
