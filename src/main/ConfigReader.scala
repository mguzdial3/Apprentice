package main

import parse._
import data._
import xml._
import javanlp._
import java.io._
import java.util.Properties
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import edu.stanford.nlp.trees.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord;

class ConfigReader(val configFile: String) {

  val properties = new Properties()
  val in = new FileInputStream(configFile)
  properties.load(in)
  in.close()
  println(properties.getProperty("parameters"))

  def allParameters(): Array[Properties] =
    {
      var params: Array[Properties] = new Array[Properties](0)
      val paraNames = properties.getProperty("parameters").split(",")
      paraNames foreach { n =>
        val name = n.trim
        // for each parameter listed, we get a list of values
        val values = properties.getProperty(name).split(",")
        if (params.isEmpty) {
          params = values.map { v =>
            val p = new Properties()
            p.setProperty(name, v)
            p
          }
        } else {
          // for each existing Properties object, we append this parameter
          params = params flatMap { param =>
            values.map { v =>
              val p = param.clone().asInstanceOf[Properties]
              p.setProperty(name, v)
              p
            }
          }
        }
      }

      params
    }

  def initData(): (List[Story], List[Cluster]) =
    {
      val storyFile = properties.getProperty("storyFile")
      val clusterFile = properties.getProperty("clusterFile")

      println("using story file: " + storyFile)
      var storyList: List[Story] = SimpleParser.parseStories(storyFile)

      //      println("old Stories \n " + storyList.map {
      //        story =>
      //          story.members.map {
      //            sentence: Sentence => sentence.id + " " + sentence.tokens.map(_.word).mkString(" ")
      //          }.mkString("\n")
      //      }.mkString("\n###\n"))

      // val text = storyList.map(_.members.mkString("\n")).mkString("\n")
      //println(text)

      storyList = parseSentence(storyList)
      //      
      //      println("\n new Stories \n " + storyList.map {
      //        story =>
      //          story.members.map {
      //            sentence: Sentence => sentence.tokens.map(_.word).mkString(" ")
      //          }.mkString("\n")
      //      }.mkString("\n###\n"))

      println("using cluster file: " + clusterFile)
      val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

      (storyList, clusterList)
    }

  /**
   * initializing the clusters. assigning the sentences to the right story and cluster, so we
   * do not create duplicate sentence objects.
   *
   */
  def initClusters(storyList: List[Story], clusterFile: String) =
    {
      val hashmap = new HashMap[Int, Sentence]
      storyList foreach {
        story =>
          story.members foreach
            {
              sentence =>
                if (hashmap.contains(sentence.id)) throw new ParsingException("sentence repeated" + sentence.id)
                hashmap += ((sentence.id, sentence))
            }
      }

      GoldParser.parseClusters(clusterFile) map {
        c =>
          val newMembers = c.members map
            {
              sentence =>
                // make sure we get the same sentence 
                hashmap.get(sentence.id).get
            }
          val newC = new Cluster(c.name, newMembers)
          newC.members foreach { s =>
            s.cluster = newC
          }
          newC
      }
    }

  def parseSentence(storyList: List[Story]): List[Story] =
    {
      //    val sentences
      val text = storyList.map {
        story =>
          story.members.map {
            sentence: Sentence => sentence.tokens.map(_.word).mkString(" ")
          }.mkString("\n")
      }.mkString("\n")
      //println("text = " + text)
      NLPWrapper.init()
      NLPWrapper.getParsed(text)

      var allSents = storyList flatMap { _.members } toArray
      var sentBuffer = new ListBuffer[Sentence]()
      var i = 0

      while (NLPWrapper.hasNextSentence) {
        NLPWrapper.processNextSentence()
        var tokensArray = NLPWrapper.getTokens()
        if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period

          var tokenBuffer = new ListBuffer[Token]()
          for (i <- 0 to tokensArray.length - 1) {
            val t = tokensArray(i)
            tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
          }

          val tokens = tokenBuffer.toArray
          println(i + ": " + tokens.mkString(" "))
          val tree = NLPWrapper.getParseTree()
          val graph = NLPWrapper.getSemanticGraph()

          val relations = graphToRelations(graph, tokens)
          sentBuffer += Sentence(allSents(i).id, tokens, tree, relations)
          //        println("old sentence " + i + " " + allSents(i).tokens.map(_.word).mkString(" "))
          println(tokens.mkString(" "))
          println(relations.mkString("\n"))
          i += 1 // going through two lists simultaneously
        }
      }

      var allNewSents = sentBuffer.toList
      var storyBuffer = new ListBuffer[Story]()

      storyList.foreach { story =>
        val length = story.members.length
        storyBuffer += new Story(allNewSents.take(length).toArray)
        allNewSents = allNewSents.drop(length)
      }

      storyBuffer.toList
    }

  def graphToRelations(graph: SemanticGraph, tokens: Array[Token]): List[Dependency] = {
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

object ConfigReader {
  def main(args: Array[String]) {
    val reader = new ConfigReader("runconfig.txt")
    val (stories, clusters) = reader.initData()
    val xml = stories.flatMap{_.members}.map { _.toXML }.mkString("\n")
    val filename = "movieParse.xml"
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(filename)))
    pw.println("<Root>")
    pw.println(xml)
    pw.println("</Root>")
    pw.close()
    println("finished printing to file: " + filename)
    //    val parameters = reader.allParameters()
    //    parameters foreach { para =>
    ////      println(para)
    ////      println(para.getProperty("confThresholds"))
    ////      println(para.containsKey("confThresholds"))
    //      val gen = new GraphGenerator(stories, clusters, para)
    //      gen.generate()
    //    }
  }
}