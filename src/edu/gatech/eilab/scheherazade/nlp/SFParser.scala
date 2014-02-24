package edu.gatech.eilab.scheherazade.nlp

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.javanlp._
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord
import scala.collection.mutable.ListBuffer

class ParsingException(message: String) extends Exception(message)

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

  def parseMultiple(text: String): TextSnippet = {
    var count = 1
    
    nlp.getParsed(text)

    if (!nlp.hasNextSentence()) {
      throw new ParsingException("parsing " + text + " failed.")
    }

    val sentList = new ListBuffer[Sentence]()

    var exist = false
    while (nlp.hasNextSentence()) {
      try {
        sentList += getSentence(nlp, count, 0)
        count += 1
        exist = true
      } catch {
        case ex: ParsingException =>
          if (exist) {
            // if we already have one sentence, just issue a warning
            System.err.println("WARNING: " + ex.getMessage())
          } else {
            throw ex
          }
      }

    }

    TextSnippet(1, sentList.toList)
  }

  def parse(sentence: String): Sentence = parse(1, sentence)

  /**
   * parse a freeform text and turn it into a sentence object
   *  The input text should contain only a single sentence
   */
  def parse(id: Int, sentence: String): Sentence = {
    val text = sentence.trim
    if (!properEnding.exists(end => text.endsWith(end))) {
      throw new ParsingException("sentence \"" + sentence + " \" does not end with a proper ending")
    }

    nlp.getParsed(text)

    if (!nlp.hasNextSentence()) {
      throw new ParsingException("parsing sentence" + text + " failed.")
    }

    getSentence(nlp, id, 0)
  }

  def parse(sentence: Sentence): Sentence = {
    if (!properEnding(sentence)) {
      throw new ParsingException("sentence \"" + sentence.toSimpleString() + " \" does not end with a proper ending")
    }

    val text = sentence.tokens.map(_.word).mkString(" ") + "\n"
    nlp.getParsed(text)

    if (!nlp.hasNextSentence()) {
      throw new ParsingException("parsing sentence" + text + " failed.")
    }

    getSentence(nlp, sentence.id, sentence.location)

  }

  def parse(storyList: List[Story]): List[Story] = {

    storyList foreach { s =>
      s.members foreach {
        sent =>
          if (!properEnding(sent))
            throw new ParsingException("sentence \"" + sent.toSimpleString() + " \" does not end with a proper ending")
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
              throw new ParsingException("parsing " + sent.id + " " + text + " failed.")
            }

            val newSentence = getSentence(nlp, sent.id, sent.location)
            count += 1

            if (nlp.hasNextSentence()) {
              throw new ParsingException("parsing " + sent.id + " " + text + " produced two sentences.")
            }

            newSentence
        }

        new Story(newMembers)
    }

    newStoryList
  }

  /**
   * parsing clusters. This is a shorthand for the other parse function. By default,
   * we do not allow multiple sentences to exist in one Sentence object.
   *
   * @input
   * clusterList a list of clusters to be parsed
   * allowMultiple a boolean variable specifying if we allow multiple sentences to exist in one Sentence object
   *
   */
  def parse(clusterList: List[Cluster])(implicit d: DummyImplicit): List[Cluster] = {

    // the dummy implicit is a workaround for JVM's lack of support for generics

    clusterList foreach { s =>
      s.members foreach {
        sent =>
          if (!properEnding(sent))
            throw new ParsingException("sentence \"" + sent.toSimpleString() + " \" does not end with a proper ending")
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
              throw new ParsingException("parsing " + sent.id + " " + text + " failed.")
            }

            var newSentence = getSentence(nlp, sent.id, sent.location)

            if (nlp.hasNextSentence()) {
              throw new ParsingException("parsing " + sent.id + " " + text + " produced two sentences.")
            }

            count += 1
            newSentence
        }

        new Cluster(cluster.name, newMembers)
    }

    newClusters
  }

  def parseSnippets(clusterList: List[Cluster])(implicit d: DummyImplicit): List[SnippetCluster] = {
    // the dummy implicit is a workaround for JVM's lack of support for generics

    clusterList foreach { s =>
      s.members foreach {
        sent =>
          if (!properEnding(sent))
            throw new ParsingException("sentence \"" + sent.toSimpleString() + " \" does not end with a proper ending")
      }
    }

    var count = 0

    val nlp = new StanfordParserWrapper()
    val newClusters = clusterList.map {
      cluster =>

        val snippets = cluster.members map {
          sent =>
            val text = sent.tokens.map(_.word).mkString(" ") + "\n"
            nlp.getParsed(text)

            if (!nlp.hasNextSentence()) {
              throw new ParsingException("parsing " + sent.id + " " + text + " failed.")
            }

            val sentList = new ListBuffer[Sentence]()

            var exist = false
            while (nlp.hasNextSentence()) {
              try {
                sentList += getSentence(nlp, count, sent.location)
                count += 1
                exist = true
              } catch {
                case ex: ParsingException =>
                  if (exist) {
                    // if we already have one sentence, just issue a warning
                    System.err.println("WARNING: " + ex.getMessage())
                  } else {
                    throw ex
                  }
              }

            }

            TextSnippet(sent.id, sentList.toList)
        }

        new SnippetCluster(cluster.name, snippets)
    }

    newClusters
  }

  private def getSentence(nlp: StanfordParserWrapper, id: Int, location: Double): Sentence =
    {
      nlp.processNextSentence()
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
        //println("parsed: " + id + " " + tokens.map(_.toString()).mkString(" "))

        Sentence(id, tokens, null, relations, location)
      } else {
        throw new ParsingException("empty sentence " + id)
      }
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