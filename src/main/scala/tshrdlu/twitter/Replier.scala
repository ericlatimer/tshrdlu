package tshrdlu.twitter

import akka.actor._
import twitter4j._

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
 * An actor that constructs replies to a given status.
 * For best results, tweet at me something related to one of the 
 * topics from the "20 Newsgroups" data
 * e.g. Religion, baseball, atheism, windows, hockey, mideast, pc hardware
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

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply via sentiments")
    val HashRE = """\b?#([^ ]*)\b""".r
    //val ReviewLinkRE = """reviews":"([^"]*)"""".r
    val ScoreRE = """"freshness":"([^"]*)","publication":"[\sA-Za-z\W ]*","quote":"([\sA-Za-z\W',\.]*)","links""".r                
 
   // val api_key="mp842nfgxevjq6wma6rh7m9k"
    val text = stripLeadMention(status.getText).toLowerCase
    
    val searchTerm = HashRE.findAllIn(text).matchData.next.group(1)
    
    //val html = Source.fromURL("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey="+ api_key + "&q=" + searchTerm + "&page_limit=1")
     
    //val s = html.mkString
    //val reviewLink = ReviewLinkRE.findAllIn(s).matchData.next.group(1)
   
    //val htmlReview  = Source.fromURL(reviewLink+"?apikey="+api_key)
    //val reviews =  htmlReview.mkString
    val reviews = getReviews(searchTerm)

    //List of "freshness" score from rotten tomatoes and corresponding quote for movie
    val score_quote: List[(String,Future[String])]= {for { ScoreRE(score,quote) <- ScoreRE findAllIn reviews} yield (score,Future{quote})}.toList

	val polarity = Sentimenter.getPolarity(Array(text)).head
	val score = if(polarity == "0") "rotten"
		    else if (polarity == "2") "none"
		    else "fresh"
	val filteredReviews = score_quote.filter(sq => sq._1 == score)
	println("tweet polarity: " + polarity)
	println("fresh score: " + score)
        if(filteredReviews.isEmpty){
		val adversary = getOpposite(score)
		val adversaryReviews = score_quote.filter(sq => sq._1 == adversary)
		if(!adversaryReviews.isEmpty){
			val (oppScores,oppReviews) = adversaryReviews.unzip
			val adversaryFuture = Future.sequence(oppReviews) 
			adversaryFuture.map(_.map(review => "I disagree... " + review).filter(_.length <= maxLength))
		}
		else{
			val neutralReviews = score_quote.filter(sq => sq._1 == "none")
			if(!neutralReviews.isEmpty){
				val (neuScores,neuReviews) = neutralReviews.unzip
				val neutralFuture = Future.sequence(neuReviews)
				neutralFuture.map(_.map(review => "I disagree... " + review).filter(_.length <= maxLength))
      			}
			else{
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
    				statusesFuture.map(_.flatMap(getText)
					.filter(_.length <= maxLength)
					.map(_.replaceAll("\"", ""))
					.filterNot(_.contains('à'))
					.filter(x => getPolarity(x) == polarity)
					)	
			}
		}
	}
	else  {
                val (scores,reviews) = filteredReviews.unzip
		val reviewsFuture: Future[Seq[String]] = Future.sequence(reviews)
	 	reviewsFuture.map(_.filter(_.length <= maxLength))
	 }
  }

  def getReviews(searchTerm: String ): String = {
	
    	val ReviewLinkRE = """reviews":"([^"]*)"""".r
    	
        val api_key="mp842nfgxevjq6wma6rh7m9k"       
	val html = Source.fromURL("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey="+ api_key + "&q=" + searchTerm + "&page_limit=1")
     
    	val s = html.mkString
   	val reviewLink = ReviewLinkRE.findAllIn(s).matchData.next.group(1)
   
    	val htmlReview  = Source.fromURL(reviewLink+"?apikey="+api_key)
   	htmlReview.mkString

  }
  
  def getOpposite(opinion: String): String = {
	if(opinion == "fresh") "rotten"
	else "fresh"

  }

  def getPolarity(tweetStr: String): String = {
	println("Checking: " + tweetStr)
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

