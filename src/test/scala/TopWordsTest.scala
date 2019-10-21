import org.scalatest.WordSpec

class TopWordsTest extends WordSpec {

  "The function TopWords.createCloud" when {
    "given an empty iterator" should {
      "produce an empty output" in {

        val result = TopWords.createCloud(
          howMany = 1,
          minLength = 1,
          lastNWords = 1,
          lines = Iterator.empty,
          ignoreList = Set.empty
        )

        assert(result.isEmpty)
      }
    }
    "given a nonempty iterator" should {
      "produce the correct nonempty output" in {
        // input data for this test case
        val data: Iterator[String] = Seq("a", "b", "c", "aa", "bb", "cc", "aa", "bb", "aa", "bb").iterator
        val result = TopWords.createCloud(
          howMany = 3,
          minLength = 2,
          lastNWords = 5,
          lines = data,
          ignoreList = Set.empty
        )

        assert(result.toList === List("bb:2 aa:2 cc:1", "bb:2 aa:2 cc:1", "bb:2 aa:2 cc:1"))
      }
    }
    "given a nonempty iterator and ignore list" should {
      "produce the correct nonempty output while ignoring words in said list" in {
        // input data for this test case
        val data: Iterator[String] = Seq("a", "b", "c", "aa", "bb", "cc", "aa", "bb", "aa", "bb", "aa").iterator
        val result = TopWords.createCloud(
          howMany = 3,
          minLength = 2,
          lastNWords = 5,
          lines = data,
          ignoreList = Set("bb")
        )

        assert(result.toList === List("aa:4 cc:1"))
      }
    }
    "given a nonempty iterator" should {
      "produce the correct nonempty output ignoring case" in {
        // input data for this test case
        val data: Iterator[String] = Seq("a", "b", "c", "AA", "bb", "CC", "aa", "BB", "aa", "bb").iterator
        val result = TopWords.createCloud(
          howMany = 3,
          minLength = 2,
          lastNWords = 5,
          lines = data,
          ignoreList = Set.empty
        )

        assert(result.toList === List("bb:2 aa:2 cc:1", "bb:2 aa:2 cc:1", "bb:2 aa:2 cc:1"))
      }
    }
  }
  "The function TopWords.config" when {
    "given zero arguments" should {
      "utilize default values" in {

        val (howMany, minLength, lastNWords) = TopWords.config(Array())
        assert(howMany === 1)
        assert(minLength === 6)
        assert(lastNWords === 1000)
      }
    }
      "given three arguments" should {
        "utilize given arguments" in {

          val (howMany, minLength, lastNWords) = TopWords.config(Array("3", "2", "5"))
          assert(howMany === 3)
          assert(minLength === 2)
          assert(lastNWords === 5)
        }
      }
        "given 2 arguments" should {
          "utilize given arguments and default values" in {

          val (howMany, minLength, lastNWords) = TopWords.config(Array("3", "2"))
          assert(howMany === 3)
          assert(minLength === 2)
          assert(lastNWords === 1000)
      }
    }
  }


}
