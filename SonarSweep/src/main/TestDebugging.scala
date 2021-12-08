import scala.io.Source

class DiveControl{
  val forward: String = "forward"
  val down: String = "down"
  val up: String = "up"
  def moveHorizontal(current: Int, move: Int): Int ={
    current + move
  }

  def moveVertical(current: Int, move: Int, direction: String): Int ={
    if (direction == up) {
      current - move
    } else {
      current + move
    }
  }

  def getNewAim(current_aim: Int, move: Int, direction: String): Int = {
    this.moveVertical(current_aim, move, direction)
  }

  def dive(coordinates: Array[String]): Int = {
    var current_horizontal = 0
    var current_vertical = 0
    var current_aim = 0
    for (coordinate <- coordinates) {
      val values = coordinate.split(" ")
      if (values(0) == "forward") {
        current_horizontal = this.moveHorizontal(current_horizontal, values(1).toInt)
        current_vertical = this.moveVertical(current_vertical, current_aim * values(1).toInt, down)
      } else {
        current_aim = this.getNewAim(current_aim, values(1).toInt, values(0))
      }
    }

    current_vertical * current_horizontal
  }
}

object Dive {
  def main(arge: Array[String]): Unit ={
    val diver: DiveControl = new DiveControl()
    val coordinates = Source.fromFile("/Users/ericrwemigabo/Projects/AOC/AOC21/SonarSweep/src/main/DataFiles/coordinates").getLines.toArray

    println("Position of the sub is: " + diver.dive(coordinates))
  }
}