package edu.gatech.eilab.scheherazade

import main._
import io._
import data._
import data.serialize._
import xml._
import javanlp._
import graph._
import similarity._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scalaj.http._
import java.io._

package nlp {

  object GoogleQuery {

    val ENGLISH = "eng_2012"
    val ENGLISH_FICTION = "eng_fiction_2012"
    val corpusID = new HashMap[String, Int]() ++ List((ENGLISH, 15), (ENGLISH_FICTION, 16))
    val unigramProb = loadProbability("unigram_prob.txt")
    val unigramFictionProb = loadProbability("unigram_prob_fiction.txt")
    val stopwords = loadStopWords()
    
    
    def main(args: Array[String]) {

      computeMaxMin()

      //var v = queryGoogle("noble_NOUN", ENGLISH_FICTION)
      //println(v)
      //      
      //      takeABreak()
      //      
      //      v = queryGoogle("fiction_NOUN", "eng_fiction_2012")
      //      print(v)
      //println(unigramProb.mkString("\n"))
    }

    def loadProbability(filename: String): HashMap[String, Double] =
      {
        val text = scala.io.Source.fromFile(filename).getLines

        val hashmap = new HashMap[String, Double]()

        text.foreach {
          l =>
            val line = l.trim
            if (line != "") {
              val p = l.split(" ")
              hashmap += ((p(0).replaceAll(",", "") -> p(1).toDouble))
            }
        }

        hashmap
      }

    def factorial(n: Int) =
      {
        var product = 1.0
        for (i <- 1 to n) {
          product = product * i
        }
        product
      }

    def computeMaxMin() {
      val dataset = "Robbery"
      Global.switchDataSet(dataset)
      Global.setConfig(new File("configRobExp.txt"))
      val reader = new ConfigReader(Global.configFile)
      var (stories, gold) = reader.initData()

      def parsed() = CachedOperation {
        SFParser.parse(stories)
      }(new File("RobExp.lzma")).flatMap(_.members)

      val sents = parsed

      for (c <- gold) yield {
        var max = Double.NegativeInfinity
        var maxSent: Sentence = null
        var min = Double.PositiveInfinity
        var minSent: Sentence = null

        var maxSimpleProb = Double.NegativeInfinity
        var maxSimpleProbSent: Sentence = null

        var minSimpleProb = Double.PositiveInfinity
        var minSimpleProbSent: Sentence = null

        println()
        println("Cluster: " + c.name)
        for (s <- c members) yield {

          val parsedSent = sents.find(x => x.id == s.id).get
          var fictionSum = 0.0
          var simpleSum = 0.0

          var count = 0

          for (tok <- parsedSent.tokens) yield {
            if (isUsefulWord(tok)) {
              val wordWithPos = tok.word + readablePOS(tok.pos)
              //println(wordWithPos)

              val prob = queryEnglishCorpus(wordWithPos)
              if (prob > 0) {
                //println("simple prob = " + prob)
                simpleSum += math.log(prob)
              }

              val fictionProb = queryFictionCorpus(wordWithPos)
              if (fictionProb > 0) {
                //println("fictional prob = " + fictionProb)
                fictionSum += math.log(fictionProb)
              }

              count += 1
            }
          }

          val diff = fictionSum - simpleSum

          val fa = math.log(factorial(count))
          fictionSum += fa
          simpleSum += fa

          //          println(simpleSum)
          //          println(fictionSum)
          //          println(diff)
          if (diff > max) {
            max = diff
            maxSent = parsedSent
          }

          if (diff < min) {
            min = diff
            minSent = parsedSent
          }

          if (simpleSum > maxSimpleProb) {
            maxSimpleProb = simpleSum
            maxSimpleProbSent = parsedSent
          }

          if (simpleSum < minSimpleProb) {
            minSimpleProb = simpleSum
            minSimpleProbSent = parsedSent
          }

        }

        println("worst fictional sentence: " + minSent.toShortString)
        println("best fictional sentence: " + maxSent.toShortString)
        println("most probable non-fictional sentence: " + maxSimpleProbSent.toShortString)
        println("least probable non-fictional sentence: " + minSimpleProbSent.toShortString)
      }
    }

    def isUsefulWord(token: data.Token): Boolean =
      {
        (!stopwords.contains(token.lemma.toLowerCase())) &&
          (token.pos == "RB" || token.pos.startsWith("N") || token.pos.startsWith("V") ||
            token.pos == "JJ" || token.pos == "RB")
      }

    def queryEnglishCorpus(wordWithPos: String): Double = {
      if (unigramProb.contains(wordWithPos)) {
        unigramProb(wordWithPos)
      } else {
        val v1 = queryGoogle(wordWithPos, ENGLISH)
        unigramProb += ((wordWithPos -> v1))

        takeABreak()

        var out = new PrintWriter(new BufferedWriter(new FileWriter("unigram_prob.txt", true)))
        out.println(wordWithPos + ", " + v1)
        out.close()
        v1
      }
    }

    def queryFictionCorpus(wordWithPos: String): Double = {
      if (unigramFictionProb.contains(wordWithPos)) {
        unigramFictionProb(wordWithPos)
      } else {
        val v1 = queryGoogle(wordWithPos, ENGLISH_FICTION)
        unigramFictionProb += ((wordWithPos -> v1))

        takeABreak()

        var out = new PrintWriter(new BufferedWriter(new FileWriter("unigram_prob_fiction.txt", true)))
        out.println(wordWithPos + ", " + v1)
        out.close()
        v1
      }
    }

    def retrieveData() {
      Global.switchDataSet("Robbery")
      val reader = new ConfigReader(Global.configFile)
      var (stories, gold) = reader.initData()
      def parsed() = CachedOperation {
        SFParser.parse(stories)
      }(Global.parseFile).flatMap(_.members)

      val sents = parsed

      val stopwords = loadStopWords()
      //println(stopwords.mkString("\n"))
      val set = HashSet[String]()
      val pos = HashSet[String]()
      for (c <- gold) yield {
        for (s <- c members) yield {

          val parsedSent = sents.find(x => x.id == s.id).get
          for (token <- parsedSent.tokens) yield {
            if (!stopwords.contains(token.lemma.toLowerCase())) {
              if (token.pos == "RB" || token.pos.startsWith("N") || token.pos.startsWith("V") || token.pos == "JJ" || token.pos == "RB") {
                val pos = readablePOS(token.pos)
                set += token.word.toLowerCase() + pos
                //pos += token.pos
              }
            }
          }
        }

      }

      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("Robbery_unigram_prob.txt")))

      for (word <- set) yield {
        println(word)
        var value = -1.0

        do {
          value = queryGoogle(word, "eng_2012")
          takeABreak()
        } while (value < 0)

        pw.print(word)
        pw.print(", ")
        pw.println(value)
        pw.flush()
      }
      pw.close
    }

    def takeABreak() {
      val interval = scala.math.random * 10000 + 20000
      Thread.sleep(interval.toInt)
    }

    /** a 15-20 minute break **/
    def takeALongBreak() {
      val interval = scala.math.random * 300000 + 900000
      Thread.sleep(interval.toInt)
    }

    def loadStopWords(): HashSet[String] = {
      val text = scala.io.Source.fromFile("stopwords2.txt").getLines

      HashSet[String]() ++= text.map(_.trim)

    }

    def readablePOS(pos: String): String =
      {
        if (pos.startsWith("N")) "_NOUN"
        else if (pos.startsWith("V")) "_VERB"
        else if (pos == "JJ") "_ADJ"
        else if (pos.startsWith("RB")) "_ADV"
        else ""
      }

    def queryGoogle(word: String, corpus: String = ENGLISH): Double =
      {
        val query = word + ":" + corpus
        val id = corpusID(corpus).toString
        val request = Http("https://books.google.com/ngrams/graph").params(("content", query), ("year_start", "1991"),
          ("year_end", "2000"), ("corpus", id), ("smoothing", "0"), ("share", ""), ("direct_url", "t1;," + query + ";,c0")).option(HttpOptions.readTimeout(10000))

        var average = -1.0

        while (average < 0) {
          try {
            val str = request.asString
            //println("http response = " + str)
            val scanner = new java.util.Scanner(str)

            //var found = false
            val numbers = Array.ofDim[Double](10)
            var dataLine: String = null

            while (scanner.hasNextLine() && dataLine == null) {
              val line = scanner.nextLine().trim
              if (line.startsWith("var data")) {
                //println(line)
                dataLine = line
              }
            }

            if (dataLine != null) {
              val startPosition = dataLine.indexOf("timeseries")
              val remainder = dataLine.substring(startPosition + 14)
              println(remainder)
              val endPosition = remainder.indexOf("]")
              if (endPosition != -1) {
                val data = remainder.substring(0, endPosition).split(",")
                average = data.map(_.trim.toDouble).sum / 10.0
              } else {
                average = 1E-10 // very small number
              }
            } else {
              average = 1E-10 // very small number
            }

            println("Queried Google Ngram (" + corpus + "): " + word + " = " + average)

          } catch {
            case e: IOException =>
              val msg = e.getMessage()
              println("Exception: " + msg)
              takeALongBreak()
            //              if (msg.startsWith("Server redirected too many") || msg.startsWith("429: Too Many Requests")) {
            //            	  takeALongBreak()
            //              }
          }
        }
        average

      }
  }
}