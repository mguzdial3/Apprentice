package edu.gatech.eilab.scheherazade.temp

import edu.gatech.eilab.scheherazade.nlp._
import java.io._

object EmoValence {
  def main(args: Array[String]) {

    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("emo.cvs")))
    val lines = scala.io.Source.fromFile("emotionalValence.txt").getLines
    for (line <- lines) {
      val arr = line.split(" ")
      val word = arr(0)
      val valence = arr(1)

      val ficProb = WordFreqCrawler.queryFictionCorpus(word)
      val engProb = WordFreqCrawler.queryEnglishCorpus(word)

      pw.println(word + "," + valence + ", " + ficProb + ", " + engProb + ", " + (ficProb / engProb) + ", " +
        ficProb * math.log(ficProb / engProb))

      pw.flush
    }
    pw.close
  }
}