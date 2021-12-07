import scala.io.Source
import scala.collection.immutable.ListMap

class Diagnostics {
  def diagnose(report_values: Array[String]): Int = {
    var processed_values: Map[String, Map[Int, Int]] = Map()
    for (report_value <- report_values) {
      val values = report_value.split("")
      processed_values = this.updateProcessedValues(processed_values, values)
    }
    // Sort based on keys to have these appear correctly while building the binary value
    val sorted_values = ListMap(processed_values.toSeq.sortBy(_._1.toInt):_*)
    this.calculatePowerConsumption(sorted_values)
  }

  def updateProcessedValues(processed_values: Map[String, Map[Int, Int]], values: Array[String]): Map[String, Map[Int, Int]] = {
    var updated_processed_values = processed_values
    var i = 0
    for (value <- values) {
      val int_value = value.toInt
      val key: String = i.toString
      // initialize keys
      if (!updated_processed_values.contains(key)) {
        updated_processed_values = updated_processed_values + (key -> Map(1 -> 0, 0 -> 0))
      }

      if (updated_processed_values.contains(key)) {
        var processed_value = updated_processed_values(key)
        val current_value = processed_value(int_value) + 1
        processed_value = processed_value + (int_value -> current_value)
        updated_processed_values = updated_processed_values + (key -> processed_value)
      }

      i += 1
    }

    updated_processed_values
  }

  def calculatePowerConsumption(values: Map[String, Map[Int, Int]]): Int = {
    println("Fetching Gamma for values: " + values.toArray.mkString("Array(", ", ", ")"))
    val keys = values.keys
    var gamma_binary_value = ""
    var epsilon_binary_value = ""
    for (key <- keys) {
      gamma_binary_value = gamma_binary_value + values(key).maxBy(_._2)._1
      epsilon_binary_value = epsilon_binary_value + values(key).minBy(_._2)._1
    }
    println("Gamma binary retrieved = " + gamma_binary_value)
    println("Epsilon binary retrieved = " + epsilon_binary_value)

    getDecimalValue(gamma_binary_value) * getDecimalValue(epsilon_binary_value)
  }

  def getDecimalValue(binary_value: String): Int = {
    println("Binary: " + binary_value + " to integer = " + Integer.parseInt(binary_value, 2))
    Integer.parseInt(binary_value, 2)
  }
}

val diagnostics: Diagnostics = new Diagnostics()
val diagnostics_report = Source.fromFile("/Users/ericrwemigabo/Projects/AOC/AOC21/SonarSweep/src/main/DataFiles/diagnostics_report").getLines.toArray
println("The power consumption of the submarine is: " + diagnostics.diagnose(diagnostics_report))