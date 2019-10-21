import io.Source
import scala.util.{Failure, Success, Try}
import sun.misc.Signal

import scala.collection.immutable.Queue

object TopWords {
  def main(args: Array[String]): Unit = {

    Signal.handle(new Signal("INT"), _ => scala.sys.exit())


    val (howMany, minLength, lastNWords) = config(args) // method to assign args to corresponding variables

    println("HOW MANY:"+howMany)
    println("MIN LENGTH:"+minLength)
    println("LAST N WORDS:"+lastNWords)

    val lines: Iterator[String] = Source.stdin.getLines
    //val lines: Iterator[String] = Iterator("aa", "bb", "cc", "aa", "bb", "aa", "bb") // hard coded testing

    val ignoreListSource = Source.fromFile("ignore_list.txt") // get file
    val ignoreList: Set[String]  = ignoreListSource.getLines.toSet // put contents of file into a set (no repetitions)
    ignoreListSource.close()

    val wordCloud = createCloud( // variable that actually holds the word cloud after considering the parameters for createCloud()
      howMany = howMany,
      minLength = minLength,
      lastNWords = lastNWords,
      lines = lines,
      ignoreList = ignoreList
    )

    wordCloud
      .foreach(println)
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

  def createCloud(howMany: Int, minLength: Int, lastNWords: Int, lines: Iterator[String], ignoreList: Set[String]): Iterator[String] = {
    lines
      .flatMap(line => line.split("(?U)[^\\p{Alpha}0-9']+"))
      .map(_.toLowerCase())
      .filter(e => !ignoreList.contains(e))
      .filter(word => word.length >= minLength) // not considering the words in the list that are have less characters than the minimum length
      .sliding(size = lastNWords, step = 1) // step = 1 means create windows one word at a time
      .map {
        words: Seq[String] =>
          words.groupBy(identity)
            .mapValues(_.size)
            .toList
            .sortBy(-_._2)
            .take(howMany)
            .map {
              case (word: String, count: Int) => s"$word:$count" // A function which takes a tuple of String and Int as input and returns a String as output.
            }
            .mkString(" ")
      }
  }
// THESE ARE THE ATTEMPTS THAT DID NOT MAKE IT.
//  def deleteMe(howMany: Int, minLength: Int, lastNWords: Int, lines: Iterator[String], ignoreList: Set[String]): String = {
//    val words: Iterator[String] = lines.flatMap(line => line.split("(?U)[^\\p{Alpha}0-9']+"))
//
//    val cloudWords = words
//      .filter(e => !ignoreList.contains(e))
//      .filter(word => word.length >= minLength) // not considering the words in the list that are have less characters than the minimum length
//      .foldLeft(Queue[String]()) {
//      case (queue, word) =>
//        val tq = if (queue.size == lastNWords) {
//          val (elem, remainingQueue) = queue.dequeue
//          remainingQueue
//        } else queue
//        val q = tq.enqueue(word)
//
//        val cloud = q.map(_.toLowerCase())
//          .groupBy(identity)
//          .mapValues(e => e.size)
//          .toList
//          .sortBy(-_._2)
//          .take(howMany)


          /*val wordCloud: List[String] = words
      .toList // iterator to IMMUTABLE list (IMMUTABLE! Allan)
      .filter(e => !ignoreList.contains(e))
      .filter(word => word.length >= minLength) // not considering the words in the list that are have less characters than the minimum length
      .takeRight(lastNWords) // getting the last N words
      .map(_.toLowerCase()) // EC
      .groupBy(e => e) // grouping together by identity
      .mapValues(e => e.size) // map that holds element -> how many elements
      .toList // convert map to list (this took forever for me to realize)
      .sortBy(-_._2) // Ordering.by(_._2).reverse
      .take(howMany) // word cloud size
          .map {
          case (word: String, count: Int) => s"$word:$count" // A function which takes a tuple of String and Int as input and returns a String as output.
        }
          .mkString(" ")
        println(cloud)
        q

    }
    ""
  }*/
}