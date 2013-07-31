package edu.gatech.eilab.scheherazade

import data.Sentence
import data.Token
import data.serialize.XStreamable



package cluster.ngram {

  class NGramData( //protected[NGramData] var text: String, 
    val sentence: Sentence,
    protected[NGramData] var ngrams: List[List[Token]], // each sub list is an ngram
    protected[NGramData] var used: Int,
    val tokens: Array[Token] // tokens within the sentence 
    ) extends Ordered[NGramData] with XStreamable[NGramData] {

    val MAX_NGRAM_LENGTH = 5 // the maximum length of an ngram in the database

    def this(sent: Sentence) = this(sent, List[List[Token]](), 0, sent.tokens)

    def remaining() = tokens.length - used

    def isComplete() = used == tokens.length

    def compare(that: NGramData): Int = {
      /*if (this.remaining > that.remaining) -1
    else if (this.remaining < that.remaining) 1
    else */
      if (NGramData.this.cost < that.cost) 1
      else if (NGramData.this.cost > that.cost) -1
      else 0
    }
    
    def getNGramsString() = {
      ngrams.map(_.map(_.word).mkString(" "))
    }

    def cost = {
      var badness = ngrams.length + 0.2 * remaining
      for (ng <- ngrams) {
        if (ng.exists(token => token.pos.startsWith("N")) && ng.exists(token => token.pos.startsWith("V"))) {
          // this ngram contains both verbs and nouns. This is a bad ngram. 
          // A good ngram should contain only nouns or only verbs
          badness += 0.3 // A tentative value. Should be less than one. A bad ngram is better than unigrams
        }
      }

      badness
    }

    override def equals(any: Any): Boolean =
      any match {
        case that: NGramData =>
          NGramData.this.sentence == that.sentence && NGramData.this.ngrams == that.ngrams
        case _ => false
      }

    override def hashCode() =
      {
        (NGramData.this.ngrams.hashCode * 23 + this.sentence.hashCode() * 31) / 17
      }

    def solutionString() = {
      //println(ngrams)
      //ngrams.map(_.map(t => t.word + "/" + t.pos).mkString(" ")).mkString(", ")
      ngrams.map(_.map(t => t.word).mkString(" ")).mkString(", ")
    }

    override def clone(): NGramData = {
      new NGramData(this.sentence, this.ngrams, this.used, this.tokens)
    }

    def clone(newNGrams: List[List[Token]], newUsed: Int) =
      {
        new NGramData(this.sentence, newNGrams, newUsed, this.tokens)
      }

    def nextSplit(ngramDB:NGramStore): List[NGramData] = {

      var list = List[NGramData]()
      val limit = math.min(MAX_NGRAM_LENGTH, tokens.length - used)

      for (i <- 0 until limit) yield { // create a number of ngrams of different length
        var ngram = List(tokens(used)) // starting with an unigram
        for (j <- (used + 1) to (used + i)) {
          ngram = ngram ::: List(tokens(j)) // extending the unigram to the desired length
        }
        //println("testing ngram = " + ngram)
        //if ((NGramData.ngramExists(ngram) && !(ngram.startsWith("John ")) && !(ngram.startsWith("Sally "))) || i == 0) {
        if (i == 0 || ngramDB.ngramExists(ngram)) {
          // when i == 0, this is a single word, and we always accept single words
          // when i > 0, we only accept it if the ngram exists in our database AND it does not start with John or Sally
          val newUsed = i + used + 1
          val newNGrams = ngram :: ngrams
          val split = clone(newNGrams, newUsed)
          list = split :: list
        }
      }
      list
    }

  }

  object NGramData {

    /**
     * create an ngram from a sentence, but does not include the last token, which is a punctuation.
     *
     */
    def NGramDropOne(sentence: Sentence) =
      {
        val n = sentence.tokens.length
        val toks = Array.ofDim[Token](n - 1)
        for (i <- 0 until n - 1) {
          toks(i) = sentence.tokens(i)
        }

        new NGramData(sentence, List[List[Token]](), 0, toks)
      }
  }
  
 
}