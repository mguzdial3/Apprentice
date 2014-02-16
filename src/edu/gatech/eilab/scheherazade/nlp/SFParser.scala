package edu.gatech.eilab.scheherazade.nlp

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.javanlp._
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord
import scala.collection.mutable.ListBuffer

/**
 * Parsing a list of stories using the Stanford Parser
 *
 */
object SFParser {

  private val nlp = new StanfordParserWrapper()
  protected val properEnding = List(".", "?\"", ".\"", "!\"", "?", "!")

  def properEnding(sent: Sentence): Boolean =
    {
      val lastword = sent.tokens.last.word.trim
      properEnding.exists(end => lastword.endsWith(end))
    }

  /** parse a freeform text and turn it into a sentence object
   *  The input text should contain only a single sentence
   */
  def parse(sentence: String): Sentence = {
    val text = sentence.trim
    if (!properEnding.exists(end => text.endsWith(end))) {
      throw new RuntimeException("sentence \"" + sentence + " \" does not end with a proper ending")
    }

    nlp.getParsed(text)

    if (!nlp.hasNextSentence()) {
      throw new RuntimeException("parsing sentence" + text + " failed.")
    }

    nlp.processNextSentence()
    var newSentence: Sentence = null

    var tokensArray = nlp.getTokens()
    if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period

      var tokenBuffer = new ListBuffer[Token]()
      for (i <- 0 to tokensArray.length - 1) {
        val t = tokensArray(i)
        tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
      }

      val tokens = tokenBuffer.toArray

      val tree = nlp.getParseTree()
      val graph = nlp.getSemanticGraph()

      val relations = graphToRelations(graph, tokens)

      newSentence = Sentence(1, tokens, null, relations, 0)
    } else {
      throw new RuntimeException("empty sentence " + text)
    }

    newSentence
  }

  def parse(sentence: Sentence): Sentence = {
    if (!properEnding(sentence)) {
      throw new RuntimeException("sentence \"" + sentence.toSimpleString() + " \" does not end with a proper ending")
    }

    val text = sentence.tokens.map(_.word).mkString(" ") + "\n"
    nlp.getParsed(text)

    if (!nlp.hasNextSentence()) {
      throw new RuntimeException("parsing sentence" + text + " failed.")
    }

    nlp.processNextSentence()
    var newSentence: Sentence = null

    var tokensArray = nlp.getTokens()
    if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period

      var tokenBuffer = new ListBuffer[Token]()
      for (i <- 0 to tokensArray.length - 1) {
        val t = tokensArray(i)
        tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
      }

      val tokens = tokenBuffer.toArray

      val tree = nlp.getParseTree()
      val graph = nlp.getSemanticGraph()

      val relations = graphToRelations(graph, tokens)

      newSentence = Sentence(sentence.id, tokens, null, relations, sentence.location)
    } else {
      throw new RuntimeException("empty sentence " + sentence.id)
    }

    newSentence
  }

  def parse(storyList: List[Story]): List[Story] = {

    storyList foreach { s =>
      s.members foreach {
        sent =>
          if (!properEnding(sent))
            throw new RuntimeException("sentence \"" + sent.toSimpleString() + " \" does not end with a proper ending")
      }
    }

    var count = 0

    val newStoryList = storyList.map {
      story =>

        val newMembers = story.members.map {
          sent =>
            val text = sent.tokens.map(_.word).mkString(" ") + "\n"
            nlp.getParsed(text)

            if (!nlp.hasNextSentence()) {
              throw new RuntimeException("parsing " + sent.id + " " + text + " failed.")
            }

            nlp.processNextSentence()
            var newSentence: Sentence = null

            var tokensArray = nlp.getTokens()
            if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period

              var tokenBuffer = new ListBuffer[Token]()
              for (i <- 0 to tokensArray.length - 1) {
                val t = tokensArray(i)
                tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
              }

              val tokens = tokenBuffer.toArray

              val tree = nlp.getParseTree()
              val graph = nlp.getSemanticGraph()

              val relations = graphToRelations(graph, tokens)
              //println("parsed: " + sent.id + " " + tokens.map(_.toString()).mkString(" "))
              count += 1
              newSentence = Sentence(sent.id, tokens, null, relations, sent.location)
            } else {
              throw new RuntimeException("empty sentence " + sent.id)
            }

            if (nlp.hasNextSentence()) {
              throw new RuntimeException("parsing " + sent.id + " " + text + " produced two sentences.")
            }

            newSentence
        }

        new Story(newMembers)
    }

    newStoryList
  }

  /**
   * parsing clusters
   *
   */
  def parse(clusterList: List[Cluster])(implicit d: DummyImplicit): List[Cluster] = {

    // the dummy implicit is a workaround for JVM's lack of support for generics

    clusterList foreach { s =>
      s.members foreach {
        sent =>
          if (!properEnding(sent))
            throw new RuntimeException("sentence \"" + sent.toSimpleString() + " \" does not end with a proper ending")
      }
    }

    var count = 0

    val nlp = new StanfordParserWrapper()
    val newClusters = clusterList.map {
      cluster =>

        val newMembers = cluster.members.map {
          sent =>
            val text = sent.tokens.map(_.word).mkString(" ") + "\n"
            nlp.getParsed(text)

            if (!nlp.hasNextSentence()) {
              throw new RuntimeException("parsing " + sent.id + " " + text + " failed.")
            }

            nlp.processNextSentence()
            var newSentence: Sentence = null

            var tokensArray = nlp.getTokens()
            if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period

              var tokenBuffer = new ListBuffer[Token]()
              for (i <- 0 to tokensArray.length - 1) {
                val t = tokensArray(i)
                tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
              }

              val tokens = tokenBuffer.toArray

              val tree = nlp.getParseTree()
              val graph = nlp.getSemanticGraph()

              val relations = graphToRelations(graph, tokens)
              //println("parsed: " + sent.id + " " + tokens.map(_.toString()).mkString(" "))
              count += 1
              newSentence = Sentence(sent.id, tokens, null, relations, sent.location)
            } else {
              throw new RuntimeException("empty sentence " + sent.id)
            }

            if (nlp.hasNextSentence()) {
              throw new RuntimeException("parsing " + sent.id + " " + text + " produced two sentences.")
            }

            newSentence
        }

        new Cluster(cluster.name, newMembers)
    }

    newClusters
  }

  /**
   * convert the Standford Semantic Graph to a list of relations
   *
   */
  def graphToRelations(graph: SemanticGraph, tokens: Array[Token]): List[Dependency] = {

    import scala.collection.mutable.Queue

    var relations = new ListBuffer[Dependency]()
    var queue = new Queue[(IndexedWord, Int)]()
    var used = List[IndexedWord]()
    val root = graph.getFirstRoot()

    queue += ((root, 0))

    while (!queue.isEmpty) {
      val (item, depth) = queue.dequeue()
      val it = graph.outgoingEdgeIterable(item).iterator

      while (it.hasNext) {
        val edge = it.next()
        val gov = edge.getGovernor()
        val dep = edge.getDependent()
        var relation = edge.getRelation()

        // makes sure each word is added to the queue only once
        if (!used.contains(dep)) {
          queue += ((dep, depth + 1))
          used = dep :: used
        }

        var specifics = relation.getSpecific()
        if (specifics == null) specifics = ""
        relations += new Dependency(tokens(gov.index() - 1), tokens(dep.index() - 1),
          relation.getShortName(), specifics, depth)

      }
    }

    relations.toList
  }
}