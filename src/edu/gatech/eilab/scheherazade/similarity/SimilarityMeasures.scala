/** This file contains the similarity measures by Resnik, Lin, etc.
 *  
 */

package edu.gatech.eilab.scheherazade.similarity

import edu.cmu.lti.lexical_db.NictWordNet

import edu.cmu.lti.ws4j.util.WS4JConfiguration;
import edu.cmu.lti.ws4j.impl._
import scala.collection.mutable.HashMap

abstract class BasicSimilarity {

  var cache = HashMap.empty[(String, String), Double]
  var count = 0
  /**
   * an abstract method for subclasses. Called by the similarity(word1, word2) method
   *
   */
  protected def sim(word1: String, word2: String): Double

  /**
   * returns the similarity of two words. With the ability to cache the results
   * Possible improvement: store the most recent 10000 results. Not needed for this small scale for now.
   */
  def similarity(word1: String, word2: String): Double = {
    val order1 = cache.get(word1, word2)
    if (order1.isDefined) return order1.get
    else {
      val order2 = cache.get(word2, word1)
      if (order2.isDefined) return order2.get
      else {
        var value = sim(word1, word2)
        //if (value > 1) throw new Exception(lemma1 + " " + lemma2 + " = " + value)
        //println(lemma1 + ", " + lemma2 + " = " + value)              
        cache.put((word1, word2), value)
        value
      }
    }
  }
}
/*
object DISCO extends SimilarityMeasure {
  import de.linguatools.disco.DISCO
  import de.linguatools.disco.ReturnDataBN
  import de.linguatools.disco.ReturnDataCol

  val discoDir = "../../en-wikipedia-20080101/"
  var disco = new DISCO(discoDir, false)

  def similarity(word1: String, word2: String): Double = {
    disco.firstOrderSimilarity(word1, word2) * math.sqrt(8)
  }

  private def existsCollocation(mainWord: String, word2: String): Option[Double] = {
    val collocation1 = disco.collocations(discoDir, mainWord)
    if (collocation1 != null)
      collocation1.find { _.word == word2 }.map { _.value.toDouble }
    else
      None
  }
}
*/
object Lin extends BasicSimilarity {

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val lin = new Lin(db)

  def similarity(word1: String, word2: String): Double = {
    lin.calcRelatednessOfWords(word1, word2)
  }
}

object Resnik extends BasicSimilarity {

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val lin = new Resnik(db)

  def similarity(word1: String, word2: String): Double = {
    lin.calcRelatednessOfWords(word1, word2)
  }
}

// Vector is not implemented in the library
object Vector extends BasicSimilarity {

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val vec = new Vector(db)

  def similarity(word1: String, word2: String): Double = {
    vec.calcRelatednessOfWords(word1, word2)
  }
}

object WuPalmer extends BasicSimilarity {

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val vec = new WuPalmer(db)

  def similarity(word1: String, word2: String): Double = {
    vec.calcRelatednessOfWords(word1, word2)
  }
}

object JiangConrath extends BasicSimilarity {

  val db = new NictWordNet();
  WS4JConfiguration.getInstance().setMFS(true)
  val vec = new JiangConrath(db)

  def similarity(word1: String, word2: String): Double = {
    vec.calcRelatednessOfWords(word1, word2)
  }
}

object DistributionalSim extends BasicSimilarity {
  import edu.gatech.eilab.scheherazade.io.DistDBConnection
  import edu.gatech.eilab.scheherazade.io.NotFoundException

  var kb: DistDBConnection = null

  def similarity(word1: String, word2: String): Double = {
    if (kb == null) kb = new DistDBConnection

    try {
      val ng1 = kb.search(word1)
      val ng2 = kb.search(word2)
      ng1.cosineSimilarity(ng2)
    } catch {
      case ne: NotFoundException => 0
    }
  }
}