package tshrdlu.twitter

import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.DefaultHttpClient

import java.net.URL
import java.io.DataOutputStream

case class Person(firstName: String, lastName: String, age: Int)

object Sentimenter {

  	def main(args: Array[String]) {
		//val polarities = getPolarity(args)
		//val tweetsAndPolarities = args.zip(polarities)
		//tweetsAndPolarities.foreach(println)

		//println("shortened url: " + shortenURL("http://goooooooooooooooooooooooogle.com"))
  	}

	def getPolarity(args: Array[String]) = {
		// create an HttpPost object
		val post = new HttpPost("http://www.sentiment140.com/api/bulkClassifyJson?appid=emoney33@gmail.com")

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

		// print the response headers
		//println("--- HEADERS ---")
		//response.getAllHeaders.foreach(arg => println(arg))

		val responseContent = response.getEntity.getContent()
		val responseContentString = scala.io.Source.fromInputStream(responseContent)
										.getLines().mkString
		//println(responseContentString)

		val allPolarities = """polarity":(\d)""".r findAllIn responseContentString
		val polarities = allPolarities.mkString(" ")
											.split("polarity\":")
											.map(_.trim)
		
		polarities.takeRight(polarities.length-1)
	}

	def shortenURL(longUrl :String) = {
		//apiKey & login of a user
		val apiKey = "R_3c86a77dd77647ff425f7bb73b7322b3"
		val login = "emoney33"

		val link ="http://api.bit.ly/v3/shorten?format=txt&login="+login+"&apiKey="+apiKey+"&longUrl="+longUrl
		val shortUrl = scala.io.Source.fromURL(link).mkString
	 	shortUrl.trim
	}
}


