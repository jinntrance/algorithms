import scala.collection.immutable.Queue

object Queen {

  def queens(n: Int): Set[Queue[Int]] = {
    def isSafe(col: Int, queens: Queue[Int]): Boolean = {
      val row = queens.length
      0 until row zip queens forall {
        case (r, c) => col != c && Math.abs(col - c) != row - r
      }
    }
    def placeQueens(k: Int): Set[Queue[Int]] = {
      if (k == 0) Set(Queue())
      else for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield queens.enqueue(col)
    }
    placeQueens(n)
  }

  def main(args: Array[String]) {
    def show(queens: Queue[Int]) = {
      val len = queens.length
      queens.map(Vector.fill(len)("-").updated(_, "X").mkString).mkString("\n")
    }
    val qs = queens(8)
    println(qs.size)
    println(qs map show mkString ("\n\n"))
  }
}

