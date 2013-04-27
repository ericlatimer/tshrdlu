import java.io.File
import scala.io.Source

        // Read in list of valid movies from the movies.txt file, converting to lower case and removing non-alphanumeric characters
        val tweets = scala.io.Source.fromFile("data/negative").getLines.toList.filter(_.length > 0)

        println("<?xml version=\"1.0\"?>\n")
        println("<dataset>")
        for (tweet <- tweets) {
            //println("tweet: " + tweet)
            val split = tweet.split("\\s")
            val text = tweet.substring(19)
            println("""<item label=""""+split(0)+"""">""")
            println("<content>"+text+"</content>")
            println("</item>")
        }           
        println("</dataset>")

