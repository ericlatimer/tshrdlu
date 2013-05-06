package tshrdlu.twitter

import io.Source

/** A class to store tokens from an input file full of topic models tokens
  *
  * It provides some lazy data structures containing said tokens
  *
  * @param inputFile the name of the file containing the tokens
  */
class TopicModeler(val inputFile: String) {
  def inputIterator = Source.fromFile(inputFile).getLines.toList

  lazy val tuples = inputIterator.map { line =>
    val split = line.split(" ")
    (split(0) -> split(1))
  }

  lazy val wordTopicsMap = tuples.groupBy(e => e._1).mapValues(e => e.map(x => x._2).toSet)

  lazy val tuplesRev = inputIterator.map { line =>
    val split = line.split(" ")
    (split(1) -> split(0))
  }

  lazy val topicWordsMap = tuplesRev.groupBy(e => e._1).mapValues(e => e.map(x => x._2).toSet)
}
