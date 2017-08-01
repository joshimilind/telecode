object queens extends App{
  def queens(n: Int) = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        def isSafe(col: Int, queens: List[Int]) = {
          val row = queens.length
          queens zip (row - 1 to 0 by -1) forall {
            case (c, r) => col != c && (row - r) != math.abs(col - c)
          }
        }
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      }
    }
    placeQueens(n)
  }
  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  println((queens(4) take 2 map show) mkString "\n")


  def isSafe(row: Int, col: Int, queens: List[Int]) =
    queens zip (row - 1 to 0 by -1) forall {
      case (r, c) => col != c && (row - r) != math.abs(col - c)
    }

}