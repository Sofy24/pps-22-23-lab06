package u06lab.code

object Solitaire extends App:
  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val width = 5
  val height = 5
  given IterableFactory = List(_).view // could switch to Set (_), LazyList (_), or List (_).view
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def findPossibleMoves(cell: (Int, Int), newPosition: (Int, Int)): Boolean =
    (cell._1 == newPosition._1 && (cell._2 - newPosition._2).abs.equals(3)) ||
      (cell._2 == newPosition._2 && (cell._1 - newPosition._1).abs.equals(3)) ||
      ((cell._1 - newPosition._1).abs.equals(2) && (cell._2 - newPosition._2).abs.equals(2))

  def placeMarks(w: Int, h: Int, n: Int): Iterable[Seq[(Int,Int)]] = n match
      case 1 => Iterable(Seq((w / 2, h / 2)))
      case _ =>
        for
          positions <- placeMarks(w, h, n - 1)
          x <- 0 until w
          y <- 0 until h
          pos = (x, y)
          if findPossibleMoves(pos, positions.head) && !positions.contains(pos)
        yield
          Seq(pos).appendedAll(positions)


  val allTheSolutions = placeMarks(width, height, width * height)
  println(allTheSolutions)

  placeMarks(width, height, width * height).foreach(s => println(render(s, width, height) + "\n"))
  println("The solutions are " + allTheSolutions.size)


    //Iterable[Solution]
  /*def placeNumbers(n: Int): Iterable[Solution] = n match
    case 1 => putFirstPosition()
    case _ =>
      for
        positions <- placeNumbers(n - 1)
      yield

    //val numberOfCells = width * height
*/



  /*def isFree(iterable: Iterable[Solution], cell: Cell): Boolean =
    iterable.indexOf(cell) >= 0*/

  /*def putFirstPosition() (using factory: IterableFactory): Iterable[Solution] =
    factory(Seq((width / 2, height / 2)))

  def isInTheGrid(cell: Cell): Boolean = cell match
    case (x, y) if x >= 0 && y >= 0 && x < width && y < height => true
    case _ => false

  def findPossibleMoves(cell: Cell)(using factory: IterableFactory): Iterable[Solution] =
    val possibleMoves = Seq((-2, -2), (2, 2), (-2, 2), (2, -2), (0, 2), (-2, 0), (2, 0), (0, -2))
    val moves :+ possibleMoves.map((x, y) => (cell._1 + x, cell._2 + y)).filter(c => isInTheGrid(c))
    factory(moves)*/

  //println(render(solution = Seq((0, 0), (2, 1)), width = 5, height = 5))