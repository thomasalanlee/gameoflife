import org.scalatest.{Matchers, path}

class GameOfLifeTestSpec extends path.FreeSpec with Matchers {

  "Game of life" - {

    "return false if fewer than two neighbours" in {
      val initialState: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, true, false), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.runGeneration(initialState) shouldBe expected
    }

    "Any live cell with two live neighbours lives on to the next generation" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(true, true, true), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false , true, false), Array(false, false, false))

      GameOfLife.runGeneration(world) shouldBe expected
    }
    "Any live cell with three live neighbours lives on to the next generation" in {
      val world: Array[Array[Boolean]] = Array(Array(true, false, false), Array(true, true, true), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(true, false, false), Array(true, true, false), Array(false, false, false))

      GameOfLife.runGeneration(world) shouldBe expected
    }

    "Overcrowding" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(true, true, true), Array(true, true, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.runGeneration(world) shouldBe expected
    }

    "reproduction" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(true, false, true), Array(true, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.runGeneration(world) shouldBe expected
    }

    "can calculate neighbours" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.calculateNeighbours(world, 0, 0).size shouldBe 3
    }
    "can calculate neighbours from bottom corner" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.calculateNeighbours(world, 2, 2).size shouldBe 3
    }
    "can calculate neighbours from center" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.calculateNeighbours(world, 1, 1).size shouldBe 8
    }
    "can calculate neighbours from center middle" in {
      val world: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))
      val expected: Array[Array[Boolean]] = Array(Array(false, false, false), Array(false, false, false), Array(false, false, false))

      GameOfLife.calculateNeighbours(world, 2, 1).size shouldBe 5
    }
  }

}

