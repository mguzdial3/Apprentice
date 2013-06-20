package edu.gatech.eilab.scheherazade
import data._
import utils.HungarianAlgo
import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import similarity._
import io.KBConnection

package similarity {
  class SimilarityOperation(val ruler: BasicSimilarity, val KB:KBConnection) {



    var min = Double.PositiveInfinity
    var max = 0.0
    
    def getSimMatrix(sentList: List[Sentence]) = 
    {      
      val matrix = Array.fill(sentList.length, sentList.length)(0.0)
      // do the preprocessing once and for all
      val sents = sentList.map { s =>
        new Sentence(s.id, s.tokens, s.parse, preprocess(s.deps), s.location)        
      }

      // compute the unnormalized similarity for all sentences
      for (i <- 0 until sents.length) {
        for (j <- i + 1 to sents.length - 1) {
          matrix(i)(j) = sentenceSimilarity(sents(i), sents(j))._1
        }
      }      

      for (i <- 0 until sents.length) {
        for (j <- i + 1 to sents.length - 1) {
          // normalize the similarities
          matrix(i)(j) = normalize(matrix(i)(j))
          // do the symmetry
          matrix(j)(i) = matrix(i)(j)
          //if (matrix(i)(j) > 1) throw new Exception(sents(i).toString() + " " + sents(j).toString + " " + matrix(i)(j)) 
        }
      }

      matrix
    }

    protected def normalize(value:Double):Double = 
    {
      (value - min) / (max - min)
    }
    
    protected def wordSimilarity(word1: Token, word2: Token): Double =
      {
        if (word1 == word2) 1
        else {
          val lemma1 = word1.lemma
          val lemma2 = word2.lemma
          var value = ruler.similarity(lemma1, lemma2)
          if (value < 0) value = 0
          if (value < min) min = value
          if (value > max) max = value
          value
        }
      }

    protected def dependencySimilarity(dep1: Dependency, dep2: Dependency): Double =
      {
        if (dep1.longName != dep2.longName) 0
        else {
          val head1 = dep1.gov.lemma
          val head2 = dep2.gov.lemma

          val tail1 = dep1.dep.lemma
          val tail2 = dep2.dep.lemma

          // this part treats John and Sally as the same person. May not apply to all situations.
          if (head1 == head2 && ((tail1 == "Sally" && tail2 == "John") || (tail2 == "Sally" && tail1 == "John")))
            return 1
          else if (tail1 == tail2 && ((head1 == "Sally" && head2 == "John") || (head2 == "Sally" && head1 == "John")))
            return 1
          else {

            // part ends
            val govSim = wordSimilarity(dep1.gov, dep2.gov)
            val depSim = wordSimilarity(dep1.dep, dep2.dep)

            var base =
              // part begins
              if (((tail1 == "Sally" && tail2 == "John") || (tail2 == "Sally" && tail1 == "John")) && govSim < 0.01) 0
              else if (((head1 == "Sally" && head2 == "John") || (head2 == "Sally" && head1 == "John")) && depSim < 0.01) 0
              else
                // part ends 
                0.5 * depSim + 0.5 * govSim

            //          if (head1 != head2 && tail1 == tail2) {
            //            val x = existsCollocation(head1, tail1)
            //            val y = existsCollocation(head2, tail1)
            //            if (x.isDefined && y.isDefined) {
            //              val common = (x.get + y.get) / 2
            //              val bonus = 0.3 * govSim * common // scale this bonus with the original similarity of the governor words
            //              base += bonus
            //              println(head1 + " and " + head2 + " have common context: " + tail1 + " adding " + bonus)
            //            }
            //          } else if (head1 == head2 && tail1 != tail2) {
            //            val x = existsCollocation(tail1, head1)
            //            val y = existsCollocation(tail2, head1)
            //            if (x.isDefined && y.isDefined) {
            //              val common = (x.get + y.get) / 2
            //              val bonus = 0.3 * depSim * common // scale this bonus with the original similarity of the dependent words
            //              base += bonus
            //              println(tail1 + " and " + tail2 + " have common context: " + head1 + " adding " + bonus)
            //            }
            //          }

            base
          }
        }
      }

    /**
     * return (Similarity, Dissimilarity). 
     * Sentence similarity is an assignment problem solved by the Hungarian algorithm
     *
     */
    protected def sentenceSimilarity(sent1: Sentence, sent2: Sentence): (Double, Double) =
      {
        //println("comparing sentences: " + sent1.id + " " + sent2.id)

        var deps1 = sent1.deps
        var deps2 = sent2.deps
        // deps1.length must be less than deps2.length
        // swap them if necessary
        if (deps1.length > deps2.length) {
          val temp = deps2
          deps2 = deps1
          deps1 = temp
        }
        //println("processsed: deps1 " + deps1.length + ". deps2 " + deps2.length)
        // this adjacency matrix represents the residual graph
        var residual = Array.fill(deps1.length, deps2.length)(0.0)
        //      // this is the current flow we have
        //      var flow = Array.fill(deps1.length, deps2.length)(0.0)

        // initializing the graph
        for (i <- 0 to deps1.length - 1) {
          for (j <- 0 to deps2.length - 1) {
            val sim = dependencySimilarity(deps1(i), deps2(j))
            residual(i)(j) = sim
            //residual(j)(i) = sim
            //println(i + "," + j + ": " + sim)
          }
        }
        val result = HungarianAlgo.hgAlgorithm(residual, "max");

        var sum: Double = 0
        var count: Double = 0

        for (i <- 0 to result.length - 1) {
          val idx1 = result(i)(0)
          val idx2 = result(i)(1)
          // the scaling factor = e ^ -0.2x
          // -2 because the minimum depth is 1
          sum += residual(idx1)(idx2) * math.pow(math.E, (deps1(idx1).depth + deps2(idx2).depth - 2) / -50)
          count += math.pow(math.E, (deps1(idx1).depth + deps2(idx2).depth - 2) / -50)
        }

        sum = (sum / count + sum) / 2
        //sum -= math.abs(deps1.length - deps2.length) * 0.1

        //println("loc1 : " + sent1.location + " loc 2: " + sent2.location)
        //val location = 0.3 - 0.6 * math.abs(sent1.location - sent2.location)

        //if (sum < 0.501) sum = 0
        if (sum < 0.1) sum = 0
        //      sum += location 
        //      if (sum < 0) sum = 0

        (sum, 0)

      }

    protected def preprocess(deps: List[Dependency]): List[Dependency] =
      {
        var result = deps

        // Preprocessing1: if there is a relation involving John, remove the same relation involving Sally 
        val suspect = deps.filter { d =>
          d.dep.word == "John"
        }.filter { dep1 => deps.exists { dep2 => dep2.gov == dep1.gov && dep2.dep.word == "Sally" && dep2.relation == dep1.relation } }

        if (suspect != Nil) {
          // remove sally's dependencies
          suspect.foreach { d =>
            result = result.filterNot(dr => dr.gov == d.gov && dr.dep.word == "Sally")
          }
          // Preprocessing 2: When John and Sally are parallel, remove anything between john and sally (usually the AND conjunctive)
          result = result.filterNot(dr => (dr.gov.word == "John" && dr.dep.word == "Sally") || (dr.gov.word == "Sally" && dr.dep.word == "John"))
        }

        // Preprocessing2: find nouns with two words.
        val nnRelations = result.filter { _.relation == "nn" }
        for (rel <- nnRelations) {
          //println(rel)
          val word = rel.dep.word + " " + rel.gov.word
          val lemma = rel.dep.lemma + " " + rel.gov.lemma
          
          if (KB.isWord(word)) {
            // this is a word

            result = result filterNot (_ == rel) map { r =>
              if (r.dep.word == rel.gov) {
                val newDep = Token(r.gov.id, word, r.dep.pos, lemma, "")
                Dependency(r.gov, newDep, r.relation, r.specific, r.depth)
              } else if (r.gov.word == rel.gov) {
                val newGov = Token(r.gov.id, word, r.gov.pos, lemma, "")
                Dependency(newGov, r.dep, r.relation, r.specific, r.depth)
              } else r
            }
          } else {
            println("noun phrase does not exist in WordNet: " + word)
          }
        }

        result
      }
  }
}