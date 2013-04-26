
import scala.io.Source

    // Regular expression used to extract ratings (freshness), reviews (quote), and links from Rotten Tomatoes search results 
    val ScoreRE = """"freshness":"([^"]*)","publication":"[\sA-Za-z\W ]+","quote":"([\sA-Za-z\W',\.]+)","links":\{("review":")?([^"]+)"?\}""".r

        // Read in list of valid movies from the movies.txt file, converting to lower case and removing non-alphanumeric characters
        val moviesList = scala.io.Source.fromFile("movies.txt").getLines.toList.map(_.toLowerCase).map(_.replaceAll("[^A-Za-z0-9 ]","")).filter(_.length > 0)

        //val out = new java.io.FileWriter("bunchOfReviews.xml")
        println("<?xml version=\"1.0\"?>\n")
        println("<dataset>")
        for (movie <- moviesList) {
            //println("movie: " + movie)
            val searchTerm = movie.replaceAll(" ","+")
            //println("searchTerm: " + searchTerm)
            // Get list of reviews of the movie from Rotten Tomatoes 
            val reviews = getReviews(searchTerm)
            //println("reviews: " + reviews)

            //println("score_quote_link: ")
            // List of "freshness" score from rotten tomatoes and corresponding quote/review and full review link (if available) for movie
            val score_quote_link: List[(String,String,String)]= {for { ScoreRE(score,quote,_,link) <- ScoreRE findAllIn reviews} yield (score,quote,link)}.toList
            //score_quote_link.foreach(println);
            for ((score,quote,_) <- score_quote_link) {
                println("""<item label=""""+score+"""">""")
                println("<content>"+quote+"</content>")
                println("</item>")
            }
        }           
        println("</dataset>")

    /**
    * Use Rotten Tomatoes API to obtain search results for given movie title (     search term)
    */
  def getReviews(searchTerm: String ): String = {
    
        val ReviewLinkRE = """reviews":"([^"]*)"""".r
        
        val api_key="mp842nfgxevjq6wma6rh7m9k"       
        val html = Source.fromURL("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey="+ api_key + "&q=" + searchTerm + "&page_limit=1")
     
        val s = html.mkString
        val matchedData = ReviewLinkRE.findAllIn(s).matchData
        if (matchedData.length > 0) {
            val reviewLink = matchedData.next.group(1)
            val htmlReview  = Source.fromURL(reviewLink+"?apikey="+api_key)
    htmlReview.mkString
        }
        else
            "none"
  }   


