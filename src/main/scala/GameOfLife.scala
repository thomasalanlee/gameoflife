import scala.util.Random

object GameOfLife {

  def runGeneration(world: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val gen1: Array[Array[Boolean]] = Array.ofDim(world.length, world.size)

    for (i <- 0 to world.size -1) {
      for (j <- 0 to world.length -1) {
        val booleans: List[Boolean] = calculateNeighbours(world, i, j).map(n => world(n._1)(n._2))
        val count: Int = booleans.count(p => p)
        println(s"cell $i,$j has $count live neighbours")
        gen1(i)(j) = count match {
          case x if x < 2 && world(i)(j) => false
          case 2 if world(i)(j) => true
          case 3 if world(i)(j) => true
          case x if x > 3 && world(i)(j) => false
          case x if x == 3 && !world(i)(j) => true
          case _ => world(i)(j)
        }
      }
    }
      gen1
  }

  def calculateNeighbours(world: Array[Array[Boolean]], x: Int, y: Int): List[(Int, Int)] = {
    val potentialNeighbours: List[(Int, Int)] = List((-1, -1), (-1, 0), (-1, 1), (1, -1), (1, 0), (1, 1), (0, -1), (0, +1))

    potentialNeighbours.foldLeft(List[(Int, Int)]())((acc, tup) => {
      val neighbour = (tup._1 + x, tup._2 + y)
      if (neighbour._1 >= 0 && neighbour._2 >= 0 && neighbour._1 <= (world.length - 1) && neighbour._2 <= (world.size - 1)) {
        acc :+ neighbour
      } else {
        acc
      }
    })
  }

}


