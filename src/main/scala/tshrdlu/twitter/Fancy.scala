package tshrdlu.twitter


/**
 * Implements the L2R_LR trained classifier 
 * Outputs the confusion matrix
 * Default Features: BOW
 * Uses the "featurizerall" if --extended flag is set
 */
object Fancy {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.{LiblinearConfig,Solver}
  import nak.util.ConfusionMatrix
  import chalk.lang.eng.Twokenize

  val wordlists = new WordLists

    // A function (with supporting regex) that reads the format of the PPA 
    // files and turns them into Examples. E.g. a line like:
    //   0 join board as director V
    // becames an Example with "V" as the label, and "join board as director"
    // as the features. Normally we'd go ahead and transform this into better
    // features, but this shows what you'd be more likely to do if reading in
    // documents.
    def readRaw(traininPairs: Seq[(String,String)]) = 
      for (pair <- traininPairs)
        yield Example(pair._1, pair._2)

  def retrain(tweet: String, polarity:String, cost:Double,extended:Boolean) = {
    val userTrainedFile = new java.io.File("userTraining.txt")
    
    val fw = new java.io.FileWriter(userTrainedFile.getName(),true);
    val bw = new java.io.BufferedWriter(fw);
    bw.write(polarity + " " + tweet+"\n");
    bw.close();

    Sentimenter.convertRatedTweetsTxt2XML("userTraining.txt", 
                        "userTraining.xml")

    Sentimenter.getSingleFile(List("trainingDatas/bestTraining.xml", "userTraining.xml"),
      "trainingDatas/combinedData.xml")
    getClassifier("trainingDatas/combinedData.xml",cost,extended)
  }

  def getClassifier(trainfile:String,cost:Double,extended:Boolean) : nak.core.IndexedClassifier[String] with nak.core.FeaturizedClassifier[String,String] = {
    //Digest training data
    val trainXML = scala.xml.XML.loadFile(trainfile)
    val allTrainingLabels = (trainXML \\ "item").map { item =>
      ((item \ "@label").text)
    }

    val allTrainingTweets = (trainXML \\ "content").map{x => x.text}
    val allTrainingPairs = allTrainingLabels.zip(allTrainingTweets).filter(x=>List("fresh","rotten").contains(x._1))

    // A featurizer that simply splits the raw inputs 
    // and attaches each token to "word" (Bag of Words)
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input)
        tokens.map{word => FeatureObservation("word"+"="+word)} 
      }
    }

    // A featurizer that simply splits the raw inputs, 
    // lowercases each and attaches each to "word" (Bag of Words)
    val featurizerLowerCase = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input).map(_.toLowerCase)
        tokens.map{word => FeatureObservation("word"+"="+word)} 
      }
    }

    // A featurizer that simply splits the raw inputs, 
    // filters out any "stop words" and attaches the remaining words
    // to "word" (Bag of Words)
    val featurizerStopWords = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input).filterNot(wordlists.stopwords)
        tokens.map{word => FeatureObservation("word"+"="+word)} 
      }
    }

   // A featurizer that extracts all bigrams from the input and
   // attaches each to "bigram"
   val featurizerBigrams = new Featurizer[String,String] {
    def apply(input: String) = {
       val tokens = Twokenize(input).toList.sliding(2).toList
            tokens.map{bigram => FeatureObservation("bigram"+"="+bigram)}
    }
  }
  
   // A featurizer that splits/tokenizes the raw inputs, lowercases each,
   // filters out the stopwords, and attaches each remaining word to "word"
   // It also determines the polarity of each remaining word and attaches each
   // of those polarities to "polarity".
   // This featurizer also adds the features created from the featurizerBigrams (above)
    val featurizerBigramsAll = new Featurizer[String,String] {
      def apply(input: String) = {
         val tokens = Twokenize(input).map(_.toLowerCase).filterNot(wordlists.stopwords)
              val sentiments = tokens.map{token => getPolarity(token)}
              val tokenSentiments = tokens.zip(sentiments)
              val firstFeatures = tokenSentiments.map{pair => 
                List(FeatureObservation("polarity"+"="+pair._2),FeatureObservation("word"+"="+pair._1))}.flatten 
         val bigrams = Twokenize(input).map(_.toLowerCase).toList.sliding(2).toList
              val secondFeatures = bigrams.map{bigram => FeatureObservation("bigram"+"="+bigram)}
         firstFeatures ++ secondFeatures
      }
    }

    // A featurizer that simply splits the raw inputs, 
    // lowercases each, removes stopwords 
    // and attaches the remaing tokens to "word" (Bag of Words)
    val featurizerStopWordsAndLC = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input).map(_.toLowerCase).filterNot(wordlists.stopwords)
        tokens.map{word => FeatureObservation("word"+"="+word)} 
      }
    }

    // A featurizer that tokenizes the raw inputs, 
    // lowercases each, removes stopwords 
    // and attaches the remaining words to "word" (Bag of Words)
    val featurizerall = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input).map(_.toLowerCase)//.filterNot(wordlists.stopwords)
        val trigrams = (List("start1", "start2") ++ tokens).toList.sliding(3).toList
        //trigrams.foreach(println)
        val tokensAndItsPrev2 = tokens.zip(trigrams)
        //tokensAndItsPrev2.foreach(println)
        val sentiments = tokensAndItsPrev2.map{tokenAndPrev2 => getPolarity(tokenAndPrev2)}
        val tokenSentiments = tokens.zip(sentiments)
        tokenSentiments.map{pair => 
          List(FeatureObservation("polarity"+"="+pair._2),FeatureObservation("word"+"="+pair._1))}.flatten 
      }
    }

    val featurizerallOld = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input).map(_.toLowerCase).filterNot(wordlists.stopwords)
        val sentiments = tokens.map{token => getPolarity(token)}
        val tokenSentiments = tokens.zip(sentiments)
        tokenSentiments.map{pair => 
          List(FeatureObservation("polarity"+"="+pair._2),FeatureObservation("word"+"="+pair._1))}.flatten 
      }
    }

    // A featurizer that tokenizes the raw inputs, 
    // determines the polarity of each, 
    // and attaches the "polarity" to each of the determined polarities
    val featurizerWordSentiments = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input)
        val sentiments = tokens.map{token => getPolarity(token.toLowerCase)}
        val tokenSentiments = tokens.zip(sentiments)
        tokenSentiments.map{pair => 
          FeatureObservation("polarity"+"="+pair._2)} 
      }
    }

    // A featurizer that tokenizes the raw inputs, 
    // determines the polarity of each token,
    // and adds both the bag of words feature and the polarity feature described above
    val featurizerWordSentimentsBOW = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input)
        val sentiments = tokens.map{token => getPolarity(token.toLowerCase)}
        val tokenSentiments = tokens.zip(sentiments)
        tokenSentiments.map{pair => 
          List(FeatureObservation("polarity"+"="+pair._2),FeatureObservation("word"+"="+pair._1))}.flatten 
      }
    }

    // Get the training examples in their raw format.  
    val rawExamples = readRaw(allTrainingPairs).toList
    //println("rawExamples: " + rawExamples.head)
    println("Done converting training to raw format at time: " + java.lang.System.currentTimeMillis())

    // Configure and train with liblinear. Here we use the (default) L2-Regularized 
    // Logistic Regression classifier with a C value of .5. 
    val config = new LiblinearConfig(cost=cost)
    val classifier = trainClassifier(config, if (extended) featurizerall else featurizer, rawExamples)
    saveClassifier(classifier, "classifier")
    println("Done setting up classifier at time: " + java.lang.System.currentTimeMillis())
    classifier
  }

  def apply(classifier:nak.core.IndexedClassifier[String] with nak.core.FeaturizedClassifier[String,String],evalfile:String,details:Boolean)  = {
    //Digest eval data
    val evalXML = scala.xml.XML.loadFile(evalfile)
    val allEvalLabels = (evalXML \\ "item").map { item =>
      ((item \ "@label").text)
    }
    val allEvalTweets = (evalXML \\ "content").map{x => x.text}
    val allEvalPairs = allEvalLabels.zip(allEvalTweets).filter(x=>List("fresh","rotten").contains(x._1))

    // Partially apply the labels to the curried 2-arg NakContext.maxLabel function 
    // to create the 1-arg maxLabelPpa function to get the best label for each example.
    def maxLabelPpa = maxLabel(classifier.labels) _

    // Make predictions on the evaluation data. Because the classifier knows about
    // featurization, we can apply the classifier directly to each example using evalRaw.
    val comparisons = for (ex <- readRaw(allEvalPairs).toList) yield 
      (ex.label, maxLabelPpa(classifier.evalRaw(ex.features)), ex.features)

    println("Done making comparisons at time: " + java.lang.System.currentTimeMillis())
    // Compute and print out the confusion matrix based on the comparisons 
    // obtained above.
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    val cm = ConfusionMatrix(goldLabels, predictions, inputs)
    
    println(cm)
    if (details) {
      println(cm.detailedOutput)
    }
    //println(predictions.head)
    predictions.head
  }

 
  // Function to determine the polarity of the passed word
  // positive (1) if the word is in the positive word list
  // negative (-1) if the word is in the negative word list
  // neutral (0) if the word is in neither list 
  def getPolarity(token: String) = {
  if (wordlists.posWords.contains(token)) 1 
    else if (wordlists.negWords.contains(token)) -1
    else 0
  }

  def negationWords = List("not")

  def getPolarity(tokenAndPrev2: (String,List[String])) = {
    val token = tokenAndPrev2._1
    val trigram = tokenAndPrev2._2

    if (wordlists.posWords.contains(token)) {
      if (negationWords.contains(trigram(0)) || negationWords.contains(trigram(1)) 
        || trigram(0).endsWith("n't") || trigram(1).endsWith("n't")
        || trigram(0).endsWith("nt") || trigram(1).endsWith("nt")) {
        //println("GOOOOOOOOOOOOOOOD Token: " + token)
        //println("trigram(0): " + trigram(0))
        //println("trigram(1): " + trigram(1))
         -1
      }
      else
        1
    } 
    else if (wordlists.negWords.contains(token)) {
      if (negationWords.contains(trigram(0)) || negationWords.contains(trigram(1)) 
        || trigram(0).endsWith("n't") || trigram(1).endsWith("n't")
        || trigram(0).endsWith("nt") || trigram(1).endsWith("nt"))
        1
      else
        -1
    }
    else 0
  }
}