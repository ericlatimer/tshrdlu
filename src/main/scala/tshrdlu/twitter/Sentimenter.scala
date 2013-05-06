package tshrdlu.twitter

import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.DefaultHttpClient

import java.net.URL
import java.io.DataOutputStream
import scala.io.Source
import java.io.File
import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,Solver}

/** An object which contains the scallop options for the Sentimenter object
  *
  */
object SentimenterOpts {

  import org.rogach.scallop._
 
  /** An apply method which contains the scallop options for the Sentimenter object
    *
    * @param args the arguments to main from the command line
    */
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
	val cost = opt[Double]("cost", short = 'c', default=Some(1.0), descr = "The cost parameter C.")
	val detailed = opt[Boolean]("detailed", short = 'd', descr = "Should output the correctly and incorrectly results please.")
	val eval = opt[List[String]]("eval", short = 'e', descr = "The files containing evalualation events.")
	val extended = opt[Boolean]("extended", short = 'x', default=Some(false), descr = "Use extended features.")
	val method = opt[String]("method", short = 'm', default=Some("L2R_LR"), descr = "The type of solver to use. Possible values: majority, lexicon, or any liblinear solver type.")
	val train = opt[List[String]]("train", short = 't', descr = "The files containing training events.")
	val verbose = opt[Boolean]("verbose", short = 'v', default=Some(false), descr = "Use extended features.")
	val help = opt[Boolean]("help", noshort = true, descr = "Show this message.")
	val version = opt[Boolean]("version", noshort = true, default=Some(false), descr = "Show version of this program.")
  }
}

/** An object which provides various functions needed throughout ReelTalk
  *
  */
object Sentimenter {

	/** A main method used for testing and debuging purposes
  	  * Eliminates the need to fire up the twitter bot for testing
  	  *
  	  * @param args the arguments to main from the command line
  	  */
  	def main(args: Array[String]) {
		val opts = SentimenterOpts(args)

		if (opts.version()) {
		println("(ELAF) Sentimenter Version 0.1.1")
		System.exit(0)
		}

		if (opts.verbose())
		println("Verbose mode enabled.")

		val trainFile = if (opts.train().length == 1) opts.train().head 
			else getSingleFile(opts.train(),"trainFile.xml")
		val evalFile = if (opts.eval().length == 1) opts.eval().head 
			else getSingleFile(opts.eval(),"evalFile.xml")					

		if (opts.method() == "majority") {
			//Majority(trainFile, evalFile, opts.detailed())
		} else if (opts.method() == "lexicon") {
			//Lexicon(evalFile, opts.detailed())
		} else {
			val classifierFile = new File("classifier")
			val classifier = if (classifierFile.exists) 
								loadClassifier[FeaturizedClassifier[String,String]]("classifier")
							 else
								Fancy.getClassifier(trainFile, opts.cost(), opts.extended())
    		Fancy(classifier, evalFile, opts.detailed())
		}
  	}

	/** An old function used in Phase 4 which relied on Sentimenter401
  	  * Used to get polarities of a batch of tweets
  	  *
  	  * @param args an array of strings which are texts from tweet statuses
  	  */
	def getOldPolarity(args: Array[String]) = {
		// create an HttpPost object
		val post = new HttpPost("http://www.sentiment140.com/api/bulkClassifyJson?appid=REMOVED")

		// set the Content-type
		post.setHeader("Content-type", "application/json")
		val tweetList  = for (arg <- args) yield 
							"{\"text\": \"" + arg + "\",\"topic\": \"movies\"}"
		val tweets = tweetList.mkString(",")
		val jsonStr = """{"data": [""" + tweets + """]}"""

		// add the JSON as a StringEntity
		post.setEntity(new StringEntity(jsonStr))

		// send the post request
		val response = (new DefaultHttpClient).execute(post)

		val responseContent = response.getEntity.getContent()
		val responseContentString = scala.io.Source.fromInputStream(responseContent)
										.getLines().mkString

		val allPolarities = """polarity":(\d)""".r findAllIn responseContentString
		val polarities = allPolarities.mkString(" ")
											.split("polarity\":")
											.map(_.trim)
		
		polarities.takeRight(polarities.length-1)
	}

	/** A function to shorten a long URL
  	  * Uses Bit.ly
  	  *
  	  * @param longUrl a string containing a URL
  	  */
	def shortenURL(longUrl :String) = {
		val file = new File("bitly.properties")
		val login = getProperty(file,"login")
		val apiKey = getProperty(file,"apiKey")

		val link ="http://api.bit.ly/v3/shorten?format=txt&login="+login+"&apiKey="+apiKey+"&longUrl="+longUrl
		try {
			val shortUrl = scala.io.Source.fromURL(link).mkString
	 		shortUrl.trim
		} catch  {
			case e: Exception => "http://bit.ly/RTELAF"
		}
	}

	/** A function to generate an xml file containing a single string (tweet text)
  	  *
  	  * @param tweet a string containing the tweet status text
  	  */
	def generateTweetXMLFile(tweet :String)  {
		val polarity = "positive"
	  	val out = new java.io.FileWriter("tweet.xml")
	  	out.write("<dataset>\n")
		out.write("""<item label="rotten">""")
        out.write("<content>"+tweet+"</content>")
        out.write("</item>")
	  	out.write("</dataset>")
		out.close
	}

	/** A function to grab properties from a properties file
  	  *
  	  * @param file the properties file
  	  * @param property the individual property key
  	  */
	def getProperty(file :File, property :String) = {
		val creds = Source.fromFile(file).getLines.toList
		val keys = for (line <- creds) yield {
			val split = line.split("=")
			if (split(0) == property)
				split(1)
			else
				None
		}
		keys.filterNot(k => k == None)(0)
	}

	/** A function which concatenates multiple files into a single file
  	  *
  	  * If multiple files are specified for training and/or evaluation data, 
  	  * create a single file in the appropriate XML format
  	  * @param filelist a list of file names
  	  * @param fileName the name of the resulting file
  	  * @return the name of the resulting output file
  	  */ 
	def getSingleFile(fileList:List[String],fileName:String) = {
		val out = new java.io.FileWriter(fileName)
		out.write("<?xml version=\"1.0\"?>\n")
		out.write("<dataset>\n")
		for (file <- fileList) {
			val lines = scala.io.Source.fromFile(file).getLines
			for (line <- lines) {
				if (!line.startsWith("<?xml version") && !line.startsWith("<dataset") && !line.startsWith("</dataset"))
					out.write(line+"\n")			
			}
		}
		out.write("</dataset>")
		out.close

		fileName
	}

	/** A function which converts a text file containing tweet and annotation 
	  * pairs into a corresponding XML file
	  * 
  	  * @param file name of the raw text file used as input
  	  * @param resultFile the name of the XML output file
  	  */
	def convertRatedTweetsTxt2XML(file: String, resultFile: String) {
		val outFile = new java.io.File(resultFile)
		val fw = new java.io.FileWriter(outFile.getName());
	    val bw = new java.io.BufferedWriter(fw);

        val tweets = scala.io.Source.fromFile(file).getLines.toList
        	.map(line => {line.splitAt(line.indexOf(" "))})	

        bw.write("<?xml version=\"1.0\"?>\n")
        bw.write("<dataset>\n")
        for (tweet <- tweets) {
            println("tweet: " + tweet)
            for (i <- 1 to 33) {
	            bw.write("""<item label=""""+tweet._1+"""">""")
	            bw.write("\n<content>"+tweet._2.trim+"</content>\n")
	            bw.write("</item>\n")
            }
        }           
        bw.write("\n</dataset>")
        bw.close();
	}

	/** A function which updates the state of previous tweets received per user 
	  * 
  	  * @param newTweet the new tweet received from the user
  	  * @param fromUser the name of the twittter user who tweeted at the bot
  	  */
	def updateUsersPreviousTweets(newTweet: String, fromUser: String) {
	    val tweets = scala.io.Source.fromFile("usersLatestTweets.txt").getLines.toList
        	.map(line => {line.splitAt(line.indexOf(" "))})
        val users = tweets.map(x => x._1)
		val outFile = new java.io.File("usersLatestTweets.txt")
		val fw = new java.io.FileWriter(outFile.getName());
	    val bw = new java.io.BufferedWriter(fw);

        for (tweet <- tweets) {
            println("tweet: " + tweet)
            if (tweet._1 != "")
	            if (tweet._1 != fromUser)
	            	bw.write(tweet._1.trim + " " + tweet._2.trim + "\n")
	            else 
	            	bw.write(tweet._1.trim + " " + newTweet.trim + "\n")
        }

        if (!users.contains(fromUser)) {
        	bw.write(fromUser.trim + " " + newTweet.trim + "\n")
        }

        bw.close();
	}
}


