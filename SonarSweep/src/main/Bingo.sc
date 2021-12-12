import scala.io.Source
import scala.language.postfixOps
import scala.util.Using
import scala.util.control.Breaks.{break, breakable}

class BingoCard() {
  var updated_rows: Map[String, Array[Int]] = Map()
  var updated_columns: Map[String, Array[Int]] = Map()
  var score = 0
  var is_already_bingo = false

  def updateCard(entry: Int): Unit = {
    for ((row_name, row) <- updated_rows) {
      updated_rows = updated_rows + (row_name -> update(row, entry))
    }

    for ((column_name, column) <- updated_columns) {
      updated_columns = updated_columns + (column_name -> update(column, entry))
    }
  }

  def getIsAlreadyBingo: Boolean = {
    is_already_bingo
  }

  def setIsAlreadyBingo(is_bingo: Boolean): Unit = {
    is_already_bingo = is_bingo
  }

  def setScore(last_entry: Int): Unit = {
    for ((row_name, row) <- updated_columns) {
      if (row.nonEmpty) {
        for (value <- row) {
          score = score + value
        }
      }
    }

    score = score * last_entry
  }

  /** Adding a row creates or updates the corresponding columns
    *
    * @param row_name row name
    * @param row_entry row entry
    */
  def addRowEntry(row_name: String, row_entry: Array[Int]): Unit = {
    updated_rows = updated_rows + (row_name -> row_entry)

    var column_index = 0
    for (entry <- row_entry) {
      addColumnEntry((column_index + 1).toString, entry)
      column_index += 1
    }

  }

  def addColumnEntry(column_name: String, column_entry: Int): Unit = {
    if (updated_columns.contains(column_name)) {
      var current_column = updated_columns(column_name)
      current_column = current_column :+ column_entry

      updated_columns = updated_columns + (column_name -> current_column)
    } else {
      updated_columns = updated_columns + (column_name -> Array(column_entry))
    }
  }

  def getScore: Int = {
    score
  }

  def isBingo: Boolean = {
    hasEmptyValues("rows", updated_rows) || hasEmptyValues(
      "columns",
      updated_columns
    )
  }

  def isValid: Boolean = {
    updated_columns.nonEmpty && updated_rows.nonEmpty
  }

  private def hasEmptyValues(
      prefix: String,
      map: Map[String, Array[Int]]
  ): Boolean = {
    for ((name, values) <- map) {
      if (values.isEmpty) {
        return true
      }
    }

    false
  }

  private def update(array: Array[Int], entry: Int): Array[Int] = {
    var new_array = Array[Int]()
    for (value <- array) {
      if (value != entry) {
        new_array = new_array :+ value
      }
    }

    new_array
  }
}

class BingoGame(game_data: Array[String]) {

  def play(): BingoCard = {
    val game_draws = getGameDraws
    println("Game draws = " + game_draws.mkString("Array(", ", ", ")"))
    val bingo_cards = buildCards()

    println("the number of bingo cards is: " + bingo_cards.length)
    var round = 0
    var winners = 0
    for (game_draw <- game_draws) {
      for (bingo_card <- bingo_cards) {
        bingo_card.updateCard(game_draw.toInt)
        if (round > 5) {
          // Only start to check after 5 rounds
          if (!bingo_card.getIsAlreadyBingo && bingo_card.isBingo) {
            bingo_card.setScore(game_draw.toInt)
            bingo_card.setIsAlreadyBingo(bingo_card.isBingo)
            if (winners == bingo_cards.length - 1) {
              return bingo_card
            }

            if (winners == 0) {
              println("The first winner score was: " + bingo_card.getScore)
            }

            winners = winners + 1
          }
        }
      }

      round = round + 1
    }

    // no winner? return first bingo card to be dealt with later.
    bingo_cards(0)
  }

  def buildCards(): Array[BingoCard] = {
    var bingo_cards = Array[BingoCard]()
    var is_new_card = false
    var current_bingo_card = new BingoCard() // initially assign an empty card
    var row_index = 0

    for (datum <- game_data) {
      if (datum != game_data(0) && datum != "") {
        val rows = formatRowData(datum)
        if (is_new_card) {
          //we add the current bingo card to the cards to be returned if it is not empty
          if (current_bingo_card.isValid) {
            bingo_cards = bingo_cards :+ current_bingo_card
          }

          row_index = 0
          current_bingo_card = new BingoCard()
          current_bingo_card.addRowEntry((row_index + 1).toString, rows)

          row_index += 1
          is_new_card = false
        } else {
          current_bingo_card.addRowEntry((row_index + 1).toString, rows)
          row_index += 1
        }
      } else {
        is_new_card = true
      }
    }

    //we add the current bingo card to the cards to be returned if it is not empty
    if (current_bingo_card.isValid) {
      bingo_cards = bingo_cards :+ current_bingo_card
    }

    bingo_cards
  }

  def formatRowData(row: String): Array[Int] = {
    val formatted_row = row.replace("  ", " 0").split(" ")
    var int_formatted_rows = Array[Int]()

    for (formatted_row <- formatted_row) {
      if (formatted_row != "") {
        int_formatted_rows = int_formatted_rows :+ formatted_row.toInt
      }
    }

    int_formatted_rows
  }

  def getGameDraws: Array[String] = {
    game_data(0).split(",")
  }
}

val bingo_game: BingoGame = new BingoGame(
  Source
    .fromFile(
      "/Users/ericrwemigabo/Projects/AOC/AOC21/SonarSweep/src/main/DataFiles/bingo_card"
    )
    .getLines
    .toArray
)
val winning_card = bingo_game.play()
println("The last winning score is: " + winning_card.getScore)
