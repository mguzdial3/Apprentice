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

  object SentenceSelection {

    val unigramProb = loadProbability("unigram_prob.txt")
    val unigramFictionProb = loadProbability("unigram_prob_fiction.txt")
    val stopwords = loadStopWords()

    def main(args: Array[String]) {

      computeMaxMin()
      //      var v = queryGoogle("fiction_NOUN", "eng_2012")
      //      println(v)
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
      val dataset = "Airport"
      Global.switchDataSet(dataset)

      val reader = new ConfigReader(Global.configFile)
      var (stories, gold) = reader.initData()

      def parsed() = CachedOperation {
        SFParser.parse(stories)
      }(Global.parseFile).flatMap(_.members)

      val sents = parsed

      for (c <- gold) {
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
        for (s <- c.members) {

          val parsedSent = sents.find(x => x.id == s.id).get
          var fictionSum = 0.0
          var simpleSum = 0.0

          var count = 0

          for (tok <- parsedSent.tokens) {
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
        val v1 = queryGoogle(wordWithPos, "eng_2012")
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
        val v1 = queryGoogle(wordWithPos, "eng_fiction_2012")
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
      for (c <- gold) {
        //val hashmap = scala.collection.mutable.HashMap[String, Int]()

        for (s <- c.members) {

          val parsedSent = sents.find(x => x.id == s.id).get
          for (token <- parsedSent.tokens) {
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

      for (word <- set) {
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
      val interval = scala.math.random * 5000 + 15000
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

    def queryGoogle(word: String, corpus: String = "eng_2012"): Double =
      {
        val query = word + ":" + corpus
        val request = Http("http://books.google.com/ngrams/graph").params(("content", query), ("year_start", "1991"),
          ("year_end", "2000"), ("corpus", "15"), ("smoothing", "0")).option(HttpOptions.readTimeout(10000))

        var average = -1.0
        
        while (average < 0) {
          try {
            val str = request.asString

            val scanner = new java.util.Scanner(str)

            var found = false
            val numbers = Array.ofDim[Double](10)

            while (scanner.hasNextLine() && !found) {
              val line = scanner.nextLine().trim
              if (line == "data.addRows(") {
                val dataLine = scanner.nextLine.trim
                val data = dataLine.replaceAll("\\]", "").replaceAll("\\[", "").split(",")
                for (i <- 1 until data.length by 2) {
                  numbers((i - 1) / 2) = data(i).trim.toDouble
                }
                //println(numbers.mkString(", "))
                found = true
              }
            }

            average = numbers.sum / 10.0
            println("Queried Google Ngram (" + corpus + "): " + word + " = " + average)

          } catch {
            case e: Exception =>
              println(e.getMessage())
              takeABreak()
          }
        }
        average

      }
  }
}