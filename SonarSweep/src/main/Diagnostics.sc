import scala.io.Source
import scala.collection.immutable.ListMap

class Diagnostics {
  def diagnose(report_values: Array[String]): Map[String, Int] = {
    var processed_values: Map[String, Map[Int, Int]] = Map()
    for (report_value <- report_values) {
      val values = report_value.split("")
      processed_values = this.updateProcessedValues(processed_values, values)
    }
    // Sort based on keys to have these appear correctly while building the binary value
    val sorted_values = ListMap(processed_values.toSeq.sortBy(_._1.toInt):_*)

    Map(
      "Power consumption: " -> this.calculatePowerConsumption(sorted_values),
      "Life support rating" -> this.calculateLifeSupportRating(report_values)
    )
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

  def calculateLifeSupportRating(report_values: Array[String]): Int = {
    var CO2_rating_values = report_values
    var oxygen_rating_values = report_values

    while (CO2_rating_values.length > 1) {
      var processed_values: Map[String, Map[Int, Int]] = Map()
      for (report_value <- report_values) {
        val values = report_value.split("")
        processed_values = this.updateProcessedValues(processed_values, values)
      }
      // Sort based on keys to have these appear correctly while building the binary value
      val sorted_values = ListMap(processed_values.toSeq.sortBy(_._1.toInt):_*)
      CO2_rating_values = this.filterMaxValues(CO2_rating_values, sorted_values)
    }

    while (oxygen_rating_values.length > 1) {
      var processed_values: Map[String, Map[Int, Int]] = Map()
      for (report_value <- report_values) {
        val values = report_value.split("")
        processed_values = this.updateProcessedValues(processed_values, values)
      }
      // Sort based on keys to have these appear correctly while building the binary value
      val sorted_values = ListMap(processed_values.toSeq.sortBy(_._1.toInt):_*)
      oxygen_rating_values = this.filterMinValuesByKey(oxygen_rating_values, sorted_values)
    }

    println("CO2 Rating is: " + getDecimalValue(CO2_rating_values(0)))
    println("Oxygen Rating is: " + getDecimalValue(oxygen_rating_values(0)))
    getDecimalValue(CO2_rating_values(0)) * getDecimalValue(oxygen_rating_values(0))
  }

  def refreshArray(report_values: Array[String]): Map[String, Map[Int, Int]] = {
    var processed_values: Map[String, Map[Int, Int]] = Map()
      for (report_value <- report_values) {
        val values = report_value.split("")
        processed_values = this.updateProcessedValues(processed_values, values)
      }
      // Sort based on keys to have these appear correctly while building the binary value
      ListMap(processed_values.toSeq.sortBy(_._1.toInt):_*)
  }

  def filterMaxValues(report_values: Array[String], values: Map[String, Map[Int, Int]]): Array[String] = {
    var rating_values = report_values
    var updatable_values = values
    val keys = updatable_values.keys
    for (key <- keys) {
      var accepted_rating_values = Array[String]()
      val common_value = getMostCommonBinaryValue(updatable_values(key))
      if (rating_values.length > 1) {
        for (rating <- rating_values) {
          val binary_values = rating.split("")
          if (binary_values(key.toInt).toInt == common_value) {
            accepted_rating_values = accepted_rating_values :+ rating
          }
        }

        // overwrite the filtered values
        rating_values = accepted_rating_values
        updatable_values = refreshArray(rating_values)
      }
    }

    rating_values
  }

  def filterMinValuesByKey(report_values: Array[String], values: Map[String, Map[Int, Int]]): Array[String] = {
    var rating_values = report_values
    var updatable_values = values
    val keys = updatable_values.keys
    for (key <- keys) {
      var accepted_rating_values = Array[String]()
      var common_value = getMostCommonBinaryValue(updatable_values(key))
      if (common_value == 1) {
        common_value = 0
      } else {
        common_value = 1
      }

      if (rating_values.length > 1) {
        for (rating <- rating_values) {
          val binary_values = rating.split("")
          if (binary_values(key.toInt).toInt == common_value) {
            accepted_rating_values = accepted_rating_values :+ rating
          }
        }

        // overwrite the filtered values
        rating_values = accepted_rating_values
        updatable_values = refreshArray(rating_values)
      }
    }

    rating_values
  }

  def calculatePowerConsumption(values: Map[String, Map[Int, Int]]): Int = {
    println("Calculating power consumption for values: " + values.toArray.mkString("Array(", ", ", ")"))
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
    Integer.parseInt(binary_value, 2)
  }

  def getMostCommonBinaryValue(value_map: Map[Int, Int]): Int = {
    val value_0_count = value_map(0)
    val value_1_count = value_map(1)
    if (value_0_count > value_1_count) {
      0
    } else if(value_1_count > value_0_count){
      1
    } else {
      1
    }
  }
}

val diagnostics: Diagnostics = new Diagnostics()
val diagnostics_report = Source.fromFile("/Users/ericrwemigabo/Projects/AOC/AOC21/SonarSweep/src/main/DataFiles/diagnostics_report").getLines.toArray
println("The Diagnostics report for the submarine: " + diagnostics.diagnose(diagnostics_report).toArray.mkString("Array(", ", ", ")"))