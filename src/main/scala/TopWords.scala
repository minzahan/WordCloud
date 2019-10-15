import io.Source
import scala.util.{Failure, Success, Try}

object TopWords {
  def main(args: Array[String]): Unit = {

    val (howMany, minLength, lastNWords) = config(args) // method to assign args to corresponding variables

    println("HOW MANY:"+howMany)
    println("MIN LENGTH:"+minLength)
    println("LAST N WORDS:"+lastNWords)

    //val lines: Iterator[String] = Source.stdin.getLines
    val lines: Iterator[String] = Iterator("aa", "bb", "cc", "aa", "bb", "aa", "bb") // hard coded testing

    val ignoreListSource = Source.fromFile("ignore_list.txt") // get file
    val ignoreList: Set[String]  = ignoreListSource.getLines.toSet // put contents of file into a set (no repetitions)
    ignoreListSource.close()

    val wordCloud: List[String] = createCloud( // variable that actually holds the word cloud after considering the parameters for createCloud()
      howMany = howMany,
      minLength = minLength,
      lastNWords = lastNWords,
      lines = lines,
      ignoreList = ignoreList
    )

    val oneLineString = wordCloud.mkString(", ") // just formatting the output to match the example in sakai
    println(s"oneLineString = ${oneLineString}")
    wordCloud
      .foreach(print)
  }

  def config(args: Array[String]): (Int, Int, Int) = {
    val howMany = Try {
      args(0).toInt
    }.recover {
      case exp: ArrayIndexOutOfBoundsException => 1
    } match {
      case Success(value) => value
      case Failure(ex: NumberFormatException) => throw new RuntimeException("Invalid number ")
      case Failure(exception) => throw exception
    }

    val minLength = Try {
      args(1).toInt
    }.recover {
      case exp: ArrayIndexOutOfBoundsException => 6
    } match {
      case Success(value) => value
      case Failure(ex: NumberFormatException) => throw new RuntimeException("Invalid number ")
      case Failure(exception) => throw exception
    }

    val lastNWords: Int = Try {
      args(2).toInt
    }.recover {
      // args(2) or the 3rd argument is missing
      case exp: ArrayIndexOutOfBoundsException => 1000 // default value
    } match {
      case Success(value) => value
      case Failure(ex: NumberFormatException) => throw new RuntimeException("Invalid number ")
      case Failure(exception) => throw exception
    }
    (howMany, minLength, lastNWords)
  }

  def createCloud(howMany: Int, minLength: Int, lastNWords: Int, lines: Iterator[String], ignoreList: Set[String]): List[String] = {
    val words: Iterator[String] = lines.flatMap(line => line.split("(?U)[^\\p{Alpha}0-9']+"))

    val wordCloud: List[String] = words
      .toList // iterator to IMMUTABLE list (IMMUTABLE! Allan)
      .filter(e => !ignoreList.contains(e))
      .filter(word => word.length >= minLength) // not considering the words in the list that are have less characters than the mininum length
      .takeRight(lastNWords) // getting the last N words
      .map(_.toLowerCase()) // EC
      .groupBy(e => e) // grouping together by identity
      .mapValues(e => e.size) // map that holds element -> how many elements
      .toList // convert map to list
      .sortBy(-_._2) //
      .take(howMany)
      .map {
        case (word: String, count: Int) => s"$word:$count" // A function which takes a tuple of String and Int as input and returns a String as output.
      }
    wordCloud
  }
}