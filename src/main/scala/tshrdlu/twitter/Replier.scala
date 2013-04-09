package tshrdlu.twitter

import akka.actor._
import twitter4j._
import scala.language.implicitConversions

/**
 * An actor that constructs replies to a given status.
 */
trait BaseReplier extends Actor with ActorLogging {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import scala.concurrent.Future
  import akka.pattern.pipe

  def receive = {
    case ReplyToStatus(status) => 
      val replyName = status.getUser.getScreenName
      val candidatesFuture = getReplies(status, 138-replyName.length)
      candidatesFuture.map { candidates =>
        val reply = "@" + replyName + " " + candidates.toSet.head
        log.info("Candidate reply: " + reply)
        new StatusUpdate(reply).inReplyToStatusId(status.getId)
      } pipeTo sender
  }

  def getReplies(status: Status, maxLength: Int): Future[Seq[String]]

}

/**
 * An actor that constructs replies to a given status.
 */
class SynonymReplier extends BaseReplier {
  import Bot._ 
  import tshrdlu.util.English.synonymize
  import TwitterRegex._

  import context.dispatcher
  import scala.concurrent.Future

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply synonym")
    val text = stripLeadMention(status.getText).toLowerCase
    val synTexts = (0 until 10).map(_ => Future(synonymize(text))) 
    Future.sequence(synTexts).map(_.filter(_.length <= maxLength))
  }

}

/**
 * An actor that constructs replies to a given status that mentions the user's 
 * opinion on a mentioned movie.  For best results, tweet at me something  
 * that expresses a clear sentiment about a movie, including the entire movie
 * title in the text.  The actor first tries to provide a response expressing
 * the same opinion as the original tweet, then resorts to disagreeing if 
 * necessary.
 */
class SentimentReplier extends BaseReplier {
  import Bot._ 
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer
  import scala.io.Source

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  //Class copied from previous Project Phase to extract just text from tweet string
  class TweetString(s: String) {
  	def dropUpto(attr: String, extra: Int = 4) = {
	   val index = s.indexOf("\"" + attr + "\"")
	   if (index == -1) "" else s.drop(index + attr.length + extra)
	}
	def getText(attr: String) = dropUpto(attr).takeWhile(_ != '"')
	def getInt(attr: String) = dropUpto(attr,3).takeWhile(_ != ',')
  }

  //Function to get a Sequence of all tweets of the current year of the provided twitter username 
  def getPrevTweets(name: String) :Seq[String] =  {
	implicit def stringToTweetString(s: String) = new TweetString(s)

	val link = "https://api.twitter.com/1/statuses/user_timeline.json?include_entities=true&inc%E2%80%8C%E2%80%8Blude_rts=true&screen_name="+name+"&since:2013-01-01&until:2014-04-07"

	val splitTweets = scala.io.Source.fromURL(link).mkString.trim.split("\\},\\{").toList
	val tweets = splitTweets.map{x => "{"+x+"}"}
	for (tweet <- tweets) yield stringToTweetString(tweet).getText("text")
  }

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply via sentiments")

    // Get list of all previous tweets of the bot -- to prevent retweeting same review
    val prevTweets = getPrevTweets("eric_anlp").map{t => stripLeadMention(t.substring(0, 
								if (t.indexOf(" http") > 0)
								   t.indexOf(" http")
								else t.length))}
	
    // Regular expression used to extract ratings (freshness), reviews (quote), and links from Rotten Tomatoes search results 
    val ScoreRE = """"freshness":"([^"]*)","publication":"[\sA-Za-z\W ]+","quote":"([\sA-Za-z\W',\.]+)","links":\{("review":")?([^"]+)"?\}""".r

    // Get text of new status, convert to lower case, remove all non-alphanumeric characters to help with finding a movie title
    val text = stripLeadMention(status.getText).toLowerCase.replaceAll("[^A-Za-z0-9 ]","")

    // Get username of the source of the status
    val username = status.getUser().getScreenName()

    // Use Sentimenter API to get the polarity of the status (0 -> negative, 2 -> neutral, 4 -> positive)
    val polarity = Sentimenter.getPolarity(Array(text)).head
    println("Tweet polarity: " + polarity)

    // Read in list of valid movies from the movies.txt file, converting to lower case and removing non-alphanumeric characters
    val moviesList = scala.io.Source.fromFile("movies.txt").getLines.toList.map(_.toLowerCase).map(_.replaceAll("[^A-Za-z0-9 ]",""))
    
    // Determine what movies are mentioned in the status text, sort the mentioned movie titles by character length in decreasing order,
    // use the head of the resulting list as the search term for Rotten Tomatoes 
    val searchTerms = moviesList.filter(x => {("""\b"""+x+"""\b""").r.findAllIn(text).matchData.toList.length > 0}).sortBy(-_.length)

    // If no movie title is found in the text, respond like StreamReplier, but matching polarity of the original status
    if (searchTerms.isEmpty){
	defaultResponse(text,maxLength,polarity)
    }
    else{
        // Add "+" to allow for appropriate input search term for Rotten Tomatoes
	val searchTerm = searchTerms(0).replaceAll(" ","+")

    	// Get list of reviews of the movie from Rotten Tomatoes 
	val reviews = getReviews(searchTerm)
	println("Search term: " + searchTerm)

	// List of "freshness" score from rotten tomatoes and corresponding quote/review and full review link (if available) for movie
	val score_quote_link: List[(String,String,String)]= {for { ScoreRE(score,quote,_,link) <- ScoreRE findAllIn reviews} yield (score,quote,link)}.toList
	
	// List of scores paired with concatenated quotes/reviews and links
	// The URLs are shortened using the Bit.ly API  	
	val score_quote = score_quote_link.map(sql => {
					(sql._1,sql._2 + " " + Sentimenter.shortenURL(sql._3))})

	// Get separate lists of reviews+links corresponding to each polarity
	val (_,freshRev) = score_quote.filter(sq => sq._1 == "fresh").unzip
	val (_,rottenRev) = score_quote.filter(sq => sq._1 == "rotten").unzip
	val (_,neutralRev) = score_quote.filter(sq => sq._1 == "none").unzip
	val freshReviews = freshRev.map(rev => Future{rev})
	val rottenReviews = rottenRev.map(rev => Future{rev})
	val neutralReviews = neutralRev.map(rev => Future{rev})

	// Lists to determine if reviews of a given polarity exist 
	val freshCount = freshReviews.map(_.filter(_.length>1))
	val rottenCount = rottenReviews.map(_.filter(_.length>1))
	val neutralCount = neutralReviews.map(_.filter(_.length>1))

	// If negative polarity, first try to respond with a "rotten" review
	if(polarity == "0"){
		if(!rottenCount.isEmpty)
			extractResponse(rottenReviews,false,maxLength-username.length,prevTweets)
		else if(!freshCount.isEmpty)
			extractResponse(freshReviews,true,maxLength-username.length,prevTweets)
		else if(!neutralCount.isEmpty)
			extractResponse(neutralReviews,false,maxLength-username.length,prevTweets)
		else
			defaultResponse(text,maxLength,polarity)
	}
	else{
        // If positive or neutral polarity, first try to respond with a "fresh" review
		if(!freshCount.isEmpty)
			extractResponse(freshReviews,false,maxLength-username.length,prevTweets)
		else if(!rottenCount.isEmpty)
			extractResponse(rottenReviews,true,maxLength-username.length,prevTweets)
		else if(!neutralCount.isEmpty)
			extractResponse(neutralReviews,false,maxLength-username.length,prevTweets)
		else
			defaultResponse(text,maxLength,polarity)
		}	
	}
  }

  /**
   * Takes original tweet (text), maximum length of a tweet, and the 
   * original tweet's polarity as input. Queries Twitter for list of 
   * statuses including the words from the original tweet, gets "proper" tweets, 
   * and returns candidate tweets that have the same polarity as the original text  
   */
  def defaultResponse(text: String, maxL: Int, polarity:String): Future[Seq[String]] = {
	val statusSeqFutures: Seq[Future[Seq[Status]]] = 
					SimpleTokenizer(text)
					.filter(_.length > 3)
					.filter(_.length < 10)
					.filterNot(_.contains('/'))
					.filter(tshrdlu.util.English.isSafe)
					.sortBy(- _.length)
					.take(3) 
					.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])

  	// Convert this to a Future of a single sequence of candidate replies
   	val statusesFuture: Future[Seq[Status]] =
      	Future.sequence(statusSeqFutures).map(_.flatten)

 	// Filter statuses to their text and make sure they are short enough to use.
	// Filter out statuses that do not match the polarity of the original tweet.  
    	statusesFuture.map(_.flatMap(getText)
			.filter(_.length <= maxL)
			.map(_.replaceAll("\"", ""))
			.filterNot(_.contains("äöüÄÖÜßéèáàúùóò".toArray))
			.filter(x => getPolarity(x) == polarity)
			)	
 }

   /**
   * Takes a sequence of candidate reviews of the same polarity, a boolean 
   * isOpp specifying if the reply will be disagreeing (opposing) the original tweet,
   * the maximum length of the tweet, and the list of statuses already tweeted by the 
   * bot as input. If a quote/review from Rotten Tomatoes does not exceed the max 
   * length, the quote without the URL is returned as a candidate.  If *every* candidate
   * exceeds the max length, the appropriate number of characters is removed from each 
   * quote to allow for the length requirement to be met.   
   * Returns reviews/quotes that meet the length requirement and have not been already
   * posted. 
   */
  def extractResponse(reviews: Seq[Future[String]], isOpp: Boolean, maxL: Int, prevTweets: Seq[String]): Future[Seq[String]] = {
	
		val reviewFuture: Future[Seq[String]] = Future.sequence(reviews)

		if(!isOpp)
			reviewFuture.map(seq => if (seq.filter(_.length-23 <= maxL).length < 1){
							seq.map{review => {
								val split = review.split(" http")
								val link = "http" + split(1)
								val revMinusLink = review.substring(0,maxL-link.length)
								val returns = revMinusLink +" " + link
								if (!prevTweets.contains(revMinusLink))
									returns
								else
									"Filter me out please"}}
								}
						else {
							seq.map{s => s.substring(0, s.indexOf(" http"))}
								.filter(_.length <= maxL)
								.filterNot(x => prevTweets.contains(x))}).map(seq => seq.filterNot(_ == "Filter me out please"))
		else{
			// Opposing opinion, attach "I disagree..." to the beginning of the new status
			val editedFuture = reviewFuture.map(_.map(review => "I disagree... " + review))  
			editedFuture.map(seq => if (seq.filter(_.length-23 <= maxL).length < 1)
							seq.map{review => {
								val split = review.split(" http")
								val link = "http" + split(1)
								val revMinusLink = review.substring(0,maxL-link.length)
								val returns = revMinusLink +" " + link
								if (!prevTweets.contains(revMinusLink))
									returns
								else
									"Filter me out please"}}
						else {
							seq.map{s => s.substring(0, s.indexOf(" http"))}
								.filter(_.length <= maxL)
								.filterNot(x => prevTweets.contains(x))}).map(seq => seq.filterNot(_ == "Filter me out please"))
		}
	
		
  }

  /**
   * Use Rotten Tomatoes API to obtain search results for given movie title (search term)
   */
  def getReviews(searchTerm: String ): String = {
	
    	val ReviewLinkRE = """reviews":"([^"]*)"""".r
    	
        val api_key="mp842nfgxevjq6wma6rh7m9k"       
	val html = Source.fromURL("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey="+ api_key + "&q=" + searchTerm + "&page_limit=1")
     
    	val s = html.mkString
   	val reviewLink = ReviewLinkRE.findAllIn(s).matchData.next.group(1)
   
    	val htmlReview  = Source.fromURL(reviewLink+"?apikey="+api_key)
   	htmlReview.mkString

  }


  /**
   * Use Sentiment140 API to determine polarity of input text
   */
  def getPolarity(tweetStr: String): String = {
	Sentimenter.getPolarity(Array(tweetStr)).head
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }
}

/**
 * An actor that constructs replies to a given status.
 * For best results, tweet at me something related to one of the 
 * topics from the "20 Newsgroups" data
 * e.g. Religion, baseball, atheism, windows, hockey, mideast, pc hardware
 */
class TopicModelReplier extends BaseReplier {
  import Bot._ 
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  val modeler = new TopicModeler("minTopicKeys.txt")

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply via topic models")
    val text = stripLeadMention(status.getText).toLowerCase
    val statusTopicList = SimpleTokenizer(text)
				.filter(_.length > 4)
				.toSet
				.take(3)
				.toList
				.flatMap(w => modeler.wordTopicsMap.get(w))
				.flatten

	val topicWords:List[String] = statusTopicList.map(topic => 
		modeler.topicWordsMap.getOrElse(topic,Set(" "))).take(4).flatten

	val statusQueryList :List[String] = topicWords
				.filter(_.length > 4)
                .filter(_.length < 11)
	        	.sortBy(- _.length)
				.distinct
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = 
		if(statusQueryList.length <1) {
			SimpleTokenizer(text)
				.filter(_.length > 3)
				.filter(_.length < 10)
				.filterNot(_.contains('/'))
				.filter(tshrdlu.util.English.isSafe)
				.sortBy(- _.length)
				.take(3) 
				.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])}
		else { statusQueryList
    			.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])}

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      	Future.sequence(statusSeqFutures).map(_.flatten)

	statusesFuture.map{x => extractText(x, statusTopicList.toSet)}
  }

  /**
   * Go through the list of tweets, gets "proper" tweets, determines
   * topic distribution vectors of said tweets, calculates similarities
   * between original tweet and candidate tweets
   * Returns most similar tweeet
   */
  def extractText(statusList: Seq[Status], statusTopics: Set[String]) = {
    val useableTweets = statusList
      .map(_.getText)
      .map {
			case StripMentionsRE(rest) => rest
			case x => x
      }
      .filterNot(_.contains('@'))
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isEnglish)
      .filter(tshrdlu.util.English.isSafe)

    //Use topic model to select response
    val topicDistributions = for ( tweet <- useableTweets) yield {
    			SimpleTokenizer(tweet).filter(_.length > 4)
				.toSet
				.take(3)
				.toList
				.flatMap(w => modeler.wordTopicsMap.get(w))
				.flatten}
    
    val topicSimilarity = topicDistributions.map(ids => 
		ids.toSet.intersect(statusTopics).size * {
			if(statusTopics.size -ids.toSet.size ==0 ) 1 
			else (1/math.abs(statusTopics.size - ids.toSet.size)).toDouble})
    
    val topTweet = topicSimilarity.toList.zip(useableTweets).maxBy(_._1)._2

    List(topTweet)
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }
}

/**
 * An actor that constructs replies to a given status.
 */
class StreamReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status.
   */
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = SimpleTokenizer(text)
    .filter(_.length > 3)
    .filter(_.length < 10)
    .filterNot(_.contains('/'))
    .filter(tshrdlu.util.English.isSafe)
    .sortBy(- _.length)
    .take(3)
    .map(w => (context.parent ? SearchTwitter(new Query(w))).mapTo[Seq[Status]])

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      Future.sequence(statusSeqFutures).map(_.flatten)

    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.map(_.flatMap(getText).filter(_.length <= maxLength))
  }


  /**
   * Go through the list of Statuses, filter out the non-English ones and
   * any that contain (known) vulgar terms, strip mentions from the front,
   * filter any that have remaining mentions or links, and then return the
   * head of the set, if it exists.
   */
  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

}


/**
 * An actor that constructs replies to a given status based on synonyms.
 */
class SynonymStreamReplier extends StreamReplier {
  import Bot._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  import tshrdlu.util.English._
  import TwitterRegex._
  override implicit val timeout = Timeout(10000)


  override def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do synonym search")
    val text = stripLeadMention(status.getText).toLowerCase

    // Get two words from the tweet, and get up to 5 synonyms each (including the word itself).
    // Matched tweets must contain one synonym of each of the two words.

    val query:String = SimpleTokenizer(text)
      .filter(_.length > 3)
      .filter(_.length < 10)
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isSafe)
      .filterNot(tshrdlu.util.English.stopwords(_))
      .take(2).toList
      .map(w => synonymize(w, 5))
      .map(x=>x.mkString(" OR ")).map(x=>"("+x+")").mkString(" AND ")

    log.info("searched for: " + query)

    val futureStatuses = (context.parent ? SearchTwitter(new Query(query))).mapTo[Seq[Status]]

    futureStatuses.map(_.flatMap(getText).filter(_.length <= maxLength))
 }

}


/**
 * An actor that constructs replies to a given status.
 */
class BigramReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status using bigrams
   */
  lazy val stopwords = tshrdlu.util.English.stopwords_bot
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)

    val bigram = Tokenize(text)
      .sliding(2)
      .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
      .flatMap{case Vector(x,y) => List(x+" "+y)}
      .toList
      .sortBy(-_.length)

    val statusSeqFutures: Seq[Future[Seq[String]]] = bigram
      .takeRight(5)
      .map(w => (context.parent ? SearchTwitter(new Query("\""+w+"\""))).mapTo[Seq[Status]].map(_.flatMap(getText).toSeq))
    
    //statusSeqFutures.foreach(println)
    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[String]] =
      extractText(statusSeqFutures,bigram.toList)

    //statusesFuture.foreach(println)
    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.filter(_.length <= maxLength)
  }

  def extractText(statusList: Seq[Future[Seq[String]]],bigram:List[String]): Future[Seq[String]] = {
    val bigramMap = Future.sequence(statusList).map(_.flatten)
    //bigramMap.foreach(println)
    val sortedMap = bigramMap.map { tweet => {
      tweet.flatMap{ x => { 
        Tokenize(x)
          .sliding(2)
          .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .map(bg => bg.mkString(" ") -> x) toMap
      }}.filter { case (p,q) => bigram.contains(p)}
    }}

    val bigramSeq = sortedMap.map(_.map(_._2))
    bigramSeq
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

  
  def Tokenize(text: String): IndexedSeq[String]={
    val starts = """(?:[#@])|\b(?:http)"""
    text
    .replaceAll("""([\?!()\";\|\[\].,':])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
    .filterNot(x => x.startsWith(starts))
  }

}

/**
 * An actor that constructs replies to a given status.
 */
class LuceneReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do search replies by Lucene")
    val text = status.getText.toLowerCase
	  val StripLeadMentionRE(withoutMention) = text
	  val query = SimpleTokenizer(withoutMention)
	    .filter(_.length > 2)
	    .toList
	    .mkString(" ")
      val replyLucene = Lucene.read(query)
    Future(replyLucene).map(_.filter(_.length <= maxLength))
  }

}

