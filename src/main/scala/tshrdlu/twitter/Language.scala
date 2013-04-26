package tshrdlu.twitter

/**
 * Copyright 2013 Jason Baldridge
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * A helper object for creating Scala Source instances given a
 * the location of a resource in the classpath, which includes
 * files in the src/main/resources directory.
 */
object Resource {
  import java.util.zip.GZIPInputStream
  import java.io.DataInputStream

  /**
   * Read in a file as a Source, ensuring that the right thing
   * is done for gzipped files.
   */
  def asSource(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    if (location.endsWith(".gz"))
      io.Source.fromInputStream(new GZIPInputStream(stream))
    else
      io.Source.fromInputStream(stream)
  
  }

  def asStream(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    val stream2 = if (location.endsWith(".gz")) new GZIPInputStream(stream) else stream
    new DataInputStream(stream2)
  }
}

/**
 * A parent class for specific languages. Handles some common
 * functions.
 */
abstract class Language(code: String) {
  def stopwords: Set[String]
  def vocabulary: Set[String]

  lazy val resourceDir = "/lang/" + code
  def appendPath(subdir: String) = resourceDir + subdir
  def getLexicon(filename: String) = 
    Resource.asSource(appendPath("/lexicon/"+filename))
      .getLines
      .filterNot(_.startsWith(";")) // filter out comments
      .toSet

}

abstract class OtherLexica (code: String) {

  lazy val resourceDir = "/lang/" + code
  def appendPath(subdir: String) = resourceDir + subdir
  def getLexicon(filename: String) = 
    Resource.asSource(appendPath("/lexicon/"+filename))
      .getLines
      .filterNot(_.startsWith(";")) // filter out comments
      .toSet

}

class WordLists extends OtherLexica("eng") {
  lazy val posWords = getLexicon("positive-words.txt.gz").map{_.toLowerCase}
  lazy val negWords = getLexicon("negative-words.txt.gz").map{_.toLowerCase}
  lazy val stopwords = getLexicon("stopwords.english").map{_.toLowerCase}
}

