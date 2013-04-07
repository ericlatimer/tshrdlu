import scala.language.implicitConversions

class TweetString(s: String) {
  def dropUpto(attr: String, extra: Int = 4) = {
    val index = s.indexOf("\"" + attr + "\"")
    if (index == -1) "" else s.drop(index + attr.length + extra)
  }
  def getText(attr: String) = dropUpto(attr).takeWhile(_ != '"')
  def getInt(attr: String) = dropUpto(attr,3).takeWhile(_ != ',')
}

implicit def stringToTweetString(s: String) = new TweetString(s)

io.Source.fromFile(args(0)).getLines.foreach { tweet =>
  val username = tweet.getText("screen_name")
  val numFollowers = tweet.getInt("followers_count")
  val text = tweet.getText("text")
  if (text != "")
    println(username + " " + numFollowers + " " + text)
 }


