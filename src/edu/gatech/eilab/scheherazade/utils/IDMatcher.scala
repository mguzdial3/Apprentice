package edu.gatech.eilab.scheherazade
import data._
import graph._
import main._
import io._
import java.io._
import scala.collection.mutable.HashMap


package utils {
  // this little program makes sure sentence id starts from zero and align the gold cluster file with it
  object IDMatcher extends App {

    val reader = new ConfigReader("configRt.txt")
    var (stories, clusters) = reader.initData

    var map = new HashMap[Int, Int]()

    var i = 0
    stories = stories.map {
      s =>
        val sents = s.members.map { m =>
          val ns = Sentence(i, m.tokens)
          map += (m.id -> i)
          i += 1
          ns
        }
        new Story(sents)
    }

    val storyOut = new PrintWriter(new BufferedOutputStream(new FileOutputStream("RtStories.story")))
    for (s <- stories) {
      if (s.members.isEmpty) println("empty story")
      for (sent <- s.members) {
        storyOut.print(sent.id + " ")
        storyOut.println(sent.tokens.map(_.word).mkString(" "))
      }
      storyOut.println("###")
    }

    val clusterOut = new PrintWriter(new BufferedOutputStream(new FileOutputStream("RtGold.gold")))
    for (c <- clusters) {
      clusterOut.println("@ " + c.name)
      for (sent <- c.members) {
        clusterOut.print(map(sent.id) + " ")
        clusterOut.println(sent.tokens.map(_.word).mkString(" "))
      }
      clusterOut.println("###")
    }

    storyOut.close()
    clusterOut.close()
  }
}
