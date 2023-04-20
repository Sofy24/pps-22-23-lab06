package u06lab.code

object Solitaire extends App:
  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val width = 5
  val height = 5
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  //def putFirstPosition():


  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))

  println(render(solution = Seq((0, 0), (2, 1)), width = 5, height = 5))