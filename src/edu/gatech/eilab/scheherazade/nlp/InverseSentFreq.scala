package edu.gatech.eilab.scheherazade.nlp

import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap

class InverseSentFreq (desc: List[SingleDescription]) {

  private var freqMap = computeFreq(desc)

  private def computeFreq(desc: List[SingleDescription]): HashMap[String, Double] = {

    val freqmap = HashMap[String, Double]()
    val size:Double = desc.size
    
    val texts = desc.map { d =>
      d.allTokens.filter(t => NLPUtils.isUsefulPOS(t.pos)).map { t =>
        t.lemma.toLowerCase() + "/" + t.pos.substring(0, 2)
      }.mkString(" ")
    }

    val keys = texts.flatMap(x => x.split(" ")).distinct
    
    for (key <- keys)
    {
      val count = texts.count(t => t.contains(key))
      println(key + " : " + count / size)
      freqmap.update(key, count / size)
    }

    freqmap
  }
  
  def freq(token:Token):Double =
  {
    val key = token.lemma.toLowerCase() + "/" + token.pos.substring(0, 2)
    freqMap.get(key).getOrElse(0)
  }
}