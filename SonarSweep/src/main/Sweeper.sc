import scala.io.Source
import scala.language.postfixOps
import scala.util.control.Breaks.break

class Sweeper{
  def countIncrements(lines: Array[String]): Int = {
    var increment_count :Int = 0
    var previous_line = lines(0).toInt
    for (current_line <- lines) {
      val int_current_line = current_line.toInt
      if (int_current_line > previous_line) {
        increment_count = increment_count + 1
      }

      previous_line = int_current_line
    }

    increment_count
  }

  def countWindowIncrements(lines: Array[String]): Int = {
    var windowed_lines = Array[String]()
    var window: Map[Int, Int] = Map()
    var new_window_start = -1
    var start_new_window = false

    var i = 1
    while (i <= lines.length) {

      if (start_new_window) {
        start_new_window = false
      }

      val index = i - 1
      val current_line = lines(index)
      if (window.size != 3) {
        window = window + (i-1 -> current_line.toInt)
      } else if (window.size == 3) {
        val sum = sumUp(window).toString
        windowed_lines = windowed_lines ++ Array(sum)
        window = window.empty
        // subtract 3 to account for the index position and the starting
        i = i - 3
        start_new_window = true
      }

      if (i == lines.length) {
        val sum = sumUp(window).toString
        windowed_lines = windowed_lines ++ Array(sum)
      }

      i = i + 1
    }

    countIncrements(windowed_lines)
  }

  def sumUp(values: Map[Int, Int]): Int = {
    var total: Int = 0
    for ((key, value) <- values) {
      total = total + value
    }
    total
  }
}

var sweeper :Sweeper = new Sweeper()
val lines = Source.fromFile("/Users/ericrwemigabo/Projects/AOC/AOC21/SonarSweep/src/main/DataFiles/depths").getLines.toArray
println("The number of increments was: " + sweeper.countIncrements(lines))
println("The number of increments in the windows was: " + sweeper.countWindowIncrements(lines))
