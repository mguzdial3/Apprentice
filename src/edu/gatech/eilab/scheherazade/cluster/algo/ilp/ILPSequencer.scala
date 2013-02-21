package edu.gatech.eilab.scheherazade

import main._
import graph._
import data._

package cluster.algo.ilp {

  /**
   * Learning the sequence of clusters and the clusters at the same time, with the help from Jacob Eisenstein
   *
   * @author Albert Li
   */
  object ILPSequencer {
    def main(args: Array[String]) {
      // load the sentences and the gold
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      val stopwords = loadStopWords()

      var words = scala.collection.mutable.ListBuffer[String]()

      val sentences = stories.flatMap(_.members).map(_.tokens.map(t => regularize(t.word)))

      sentences foreach {
        _.foreach { word =>
          
          if (!(stopwords contains word) && ! (words contains word))
          {
           words += word 
           //println(word)
          }
        }
      }

      val m = words.size // m is the height of the matrix
      val n = sentences.size // n is the width of the matrix
      val freq = Array.ofDim[Int](m, n)
      

      var i = 0;
      var j = 0;

      words foreach { w =>
        sentences foreach { s =>
          freq(i)(j) = s.count(_ == w)
          j += 1
        }
        j = 0
        i += 1
      }
      
      printMatrix(words, sentences, freq)

    }

    def regularize(word:String):String = 
    {
      word.toLowerCase().replace(".", "").replace("?", "").replace("\"", "").replace(",", "")
    }
    
    def loadStopWords(): List[String] =
      {
        val list = scala.io.Source.fromFile("stopwords2.txt")
        list.getLines().toList
      }
    
    def printMatrix(words:Seq[String], sentences:List[Array[String]], freq:Array[Array[Int]])
    {
      import java.io._
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("freqMatrix.csv")))
      // first line: only the sentences
      pw.println(" , " + sentences.map(_.mkString("", " ", "")).mkString("", ",", ""))
      // second line to end
      
      for (i <- 0 until freq.length)
      {
        pw.println(words(i) + ", " + freq(i).mkString("", ",", ""))
      }
      
      pw.close()
    }
  }
}