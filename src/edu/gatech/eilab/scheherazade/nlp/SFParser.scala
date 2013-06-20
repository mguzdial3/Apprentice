package edu.gatech.eilab.scheherazade.nlp

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.javanlp._
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord
import scala.collection.mutable.ListBuffer

/** Parsing a list of stories using the Stanford Parser
 *  
 */
object SFParser {
    
    protected val properEnding = List(".", "?\"", ".\"", "!\"")
    
    def parse(storyList:List[Story]): List[Story] = {

      storyList foreach { s =>
        s.members foreach {
          sent =>
            val lastword = sent.tokens.last.word
            if (!properEnding.exists(end => lastword.endsWith(end)))
              throw new RuntimeException("sentence \"" + sent.toSimpleString() + " \" does not end with a proper ending")
        }
      }

      val text = storyList.flatMap { _.members.map { _.tokens.map(_.word).mkString(" ") } }.mkString("\n")
      val nlp = new NLPWrapper()
      //println("parsing: ************************************")
      //println(text)
      //println("parsed: ************************************")
      var count = 0
      
      nlp.getParsed(text)
      val newStories = storyList map { story =>
        val newSents = story.members map { sent =>

          if (!nlp.hasNextSentence()) throw new RuntimeException("parsed sentence exhausted prematurely")
          nlp.processNextSentence();

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
            //println("parsed: " + sent.id + " " + tokens.map(x => x.word).mkString(" "))
            count += 1
            Sentence(sent.id, tokens, null, relations, sent.location)
          } else throw new RuntimeException("empty sentence " + sent.id)
        }
        new Story(newSents)
      }
      println(count + " sentences parsed successfully.")
      newStories
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