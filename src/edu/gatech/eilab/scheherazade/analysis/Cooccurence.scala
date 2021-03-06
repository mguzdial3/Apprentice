package edu.gatech.eilab.scheherazade
import data._
import main._
import graph._


package analysis {
  object Cooccurence extends App {
    val reader = new ConfigReader("configNewMv.txt")
    var (stories, clusters) = reader.initDataFiltered()
    //var (stories, clusters) = reader.initData()
    //val para = reader.properties.allParameters()(0)
    //val gen = new GraphGenerator(stories, clusters, para)
    //val graph = gen.generate()._4.makeEfficient()
    //graph.draw("NMv")

    val size = clusters.size
    for (i <- 0 until size; j <- i + 1 until size) {
      val c1 = clusters(i)
      val c2 = clusters(j)

      var count = 0
      for (story <- stories) {
        if (story.members.exists(sent => c1.members.contains(sent)) && story.members.exists(sent => c2.members.contains(sent)))
          count += 1
      }
      print(c1.name + ", " + c2.name)
      
      val max = math.min(c1.size, c2.size)
      val mi = mutualInfo(c1, c2, stories)
      val pmi = ptMutualInfo(c1, c2, stories)
      //val sizeEntropy = clusterSizeDifference(c1, c2, stories)
      //val chisquare = computeChi(c1, c2, stories)
      //println(c1.name + ", " + c2.name + ", " + count + ", " + max + ", " + count.toDouble / max + ", " + (mi._1 + mi._2) + ", " + mi._1 + ", " + mi._2)
      //if ((mi._1 + mi._2) > 0.05 && mi._2 > 0)
      if (c1.size >= 0 && c2.size >= 0)
        
        println(", mi: " + (mi._1 + mi._2) + ", " + mi._1 + ", " + mi._2)// + ", p-value: " + chisquare._1 + ", " + chisquare._2 ) 
          //+ ", pmi: " + (pmi._1 + pmi._2) + ", " + pmi._1 + ", " + pmi._2)
      //}
    }

    Thread.sleep(3000)

    /*
    def computeChi(c1: Cluster, c2: Cluster, stories: List[Story]): (Double, Int) = {
      val counts = Array.ofDim[Long](2, 2)
      
      counts(0)(0) = stories.filter(story => ! (story.members.exists(c1.members.contains) || story.members.exists(c2.members.contains))).size
      counts(0)(1) = stories.filter(story => (!story.members.exists(c1.members.contains)) && story.members.exists(c2.members.contains)).size
      counts(1)(0) = stories.filter(story => (!story.members.exists(c2.members.contains)) && story.members.exists(c1.members.contains)).size
      counts(1)(1) = stories.filter(story => story.members.exists(c1.members.contains) && story.members.exists(c2.members.contains)).size
      
      var coOccur = if (counts(1)(1)+counts(0)(0) > counts(0)(1)+counts(1)(0)) 1 else 0
      var low = 0
      for(i <- 0 to 1; j <- 0 to 1)
      {
        print("," + counts(i)(j))
        if (counts(i)(j) < 5) low += 1
      }
        
      if (low > 1) coOccur = -1
      
      val pValue = new ChiSquareTest().chiSquareTest(counts)
      (pValue, coOccur)
    }*/
    
    def clusterSizeDifference(c1: Cluster, c2: Cluster, stories: List[Story]): Double = {
      val joint = jointSize(c1, c2, stories)
      val p1 = probDist(c1, stories)
      val p2 = probDist(c2, stories)
      val total = stories.size

      var sum1 = 0.0
      var sum2 = 0.0

      val comp1 = if (joint(0)(0) == 0) 0 else joint(0)(0) * math.log(joint(0)(0) / p1(0) / p2(0))
      val comp2 = if (joint(1)(1) == 0) 0 else joint(1)(1) * math.log(joint(1)(1) / p1(1) / p2(1))
      sum1 = comp1 + comp2

      //println("component 1:" + comp1 + " vs " + p1(0) * math.log(1/p2(0)))
      //println("component 2:" + comp2 + " vs " + p1(1) * math.log(1/p2(1)))

      val comp3 = if (joint(0)(1) == 0) 0 else joint(0)(1) * math.log(joint(0)(1) / p1(0) / p2(1))
      val comp4 = if (joint(1)(0) == 0) 0 else joint(1)(0) * math.log(joint(1)(0) / p1(1) / p2(0))
      sum2 = comp3 + comp4
      //println("component 3:" + comp3)
      //println("component 4:" + comp4)

      //println(joint(1)(0) * math.log(joint(1)(0) / p1(1) / p2(0)))
      sum1 + sum2
    }

    /**
     * mutual information of two clusters
     *
     */
    def mutualInfo(c1: Cluster, c2: Cluster, stories: List[Story]): (Double, Double) = {
      val joint = jointDist(c1, c2, stories)
      val p1 = probDist(c1, stories)
      val p2 = probDist(c2, stories)
      val total = stories.size

      var sum1 = 0.0
      var sum2 = 0.0

      val comp1 = if (joint(0)(0) == 0) 0 else joint(0)(0) * math.log(joint(0)(0) / p1(0) / p2(0))
      val comp2 = if (joint(1)(1) == 0) 0 else joint(1)(1) * math.log(joint(1)(1) / p1(1) / p2(1))
      sum1 = comp1 + comp2

      //println("component 1:" + comp1 + " vs " + p1(0) * math.log(1/p2(0)))
      //println("component 2:" + comp2 + " vs " + p1(1) * math.log(1/p2(1)))

      val comp3 = if (joint(0)(1) == 0) 0 else joint(0)(1) * math.log(joint(0)(1) / p1(0) / p2(1))
      val comp4 = if (joint(1)(0) == 0) 0 else joint(1)(0) * math.log(joint(1)(0) / p1(1) / p2(0))
      sum2 = comp3 + comp4
      //println("component 3:" + comp3)
      //println("component 4:" + comp4)

      //println(joint(1)(0) * math.log(joint(1)(0) / p1(1) / p2(0)))
      (sum1, sum2)
    }

    /**
     * pointwise mutual information of two clusters
     *
     */
    def ptMutualInfo(c1: Cluster, c2: Cluster, stories: List[Story]): (Double, Double) = {
      val joint = jointDist(c1, c2, stories)
      val p1 = probDist(c1, stories)
      val p2 = probDist(c2, stories)
      val total = stories.size

      var sum1 = 0.0
      var sum2 = 0.0

      val comp1 = math.log(joint(0)(0) / p1(0) / p2(0))
      val comp2 = math.log(joint(1)(1) / p1(1) / p2(1))
      sum1 = comp1 + comp2

      //println("component 1:" + comp1 + " vs " + p1(0) * math.log(1/p2(0)))
      //println("component 2:" + comp2 + " vs " + p1(1) * math.log(1/p2(1)))

      val comp3 = math.log(joint(0)(1) / p1(0) / p2(1))
      val comp4 = math.log(joint(1)(0) / p1(1) / p2(0))
      sum2 = comp3 + comp4
      //println("component 3:" + comp3)
      //println("component 4:" + comp4)

      //println(joint(1)(0) * math.log(joint(1)(0) / p1(1) / p2(0)))
      (sum1, sum2)
    }

    val INFINISMALL: Double = 0.0001

    private def jointSize(c1: Cluster, c2: Cluster, stories: List[Story]): Array[Array[Double]] = {
      val joint = Array.ofDim[Double](2, 2)
      val total = stories.size
      // neither happens
      joint(0)(0) = stories.filter { s =>
        (!s.members.exists(sent => c1.members.contains(sent))) &&
          (!s.members.exists(sent => c1.members.contains(sent)))
      }.size.toDouble / total

      joint(1)(0) = stories.filter { s =>
        s.members.exists(sent => c1.members.contains(sent)) &&
          (!s.members.exists(sent => c1.members.contains(sent)))
      }.size.toDouble / total

      joint(0)(1) = stories.filter { s =>
        (!s.members.exists(sent => c1.members.contains(sent))) &&
          s.members.exists(sent => c1.members.contains(sent))
      }.size.toDouble / total

      joint(1)(1) = stories.filter { s =>
        s.members.exists(sent => c1.members.contains(sent)) &&
          s.members.exists(sent => c1.members.contains(sent))
      }.size.toDouble / total

      //    for (i <- 0 to 1; j <- 0 to 1)
      //     {
      //      if (joint(i)(j) == 0) {
      //        joint(i)(j) = 0.0001
      //        //print("inf")
      //      }
      //      //println(i + " " + j + " " + joint(i)(j))
      //     }

      joint
    }

    private def jointDist(c1: Cluster, c2: Cluster, stories: List[Story]): Array[Array[Double]] = {
      val joint = Array.ofDim[Double](2, 2)
      val total = stories.size
      // neither happens
      joint(0)(0) = stories.filter { s =>
        (!s.members.exists(sent => c1.members.contains(sent))) &&
          (!s.members.exists(sent => c2.members.contains(sent)))
      }.size.toDouble / total

      joint(1)(0) = stories.filter { s =>
        s.members.exists(sent => c1.members.contains(sent)) &&
          (!s.members.exists(sent => c2.members.contains(sent)))
      }.size.toDouble / total

      joint(0)(1) = stories.filter { s =>
        (!s.members.exists(sent => c1.members.contains(sent))) &&
          s.members.exists(sent => c2.members.contains(sent))
      }.size.toDouble / total

      joint(1)(1) = stories.filter { s =>
        s.members.exists(sent => c1.members.contains(sent)) &&
          s.members.exists(sent => c2.members.contains(sent))
      }.size.toDouble / total

      //    for (i <- 0 to 1; j <- 0 to 1)
      //     {
      //      if (joint(i)(j) == 0) {
      //        joint(i)(j) = 0.0001
      //        //print("inf")
      //      }
      //      //println(i + " " + j + " " + joint(i)(j))
      //     }

      joint
    }

    private def probDist(c: Cluster, stories: List[Story]): Array[Double] = {
      var array = Array.ofDim[Double](2)
      val total = stories.size
      // probability of not happening
      // any sentence exists in that cluster => filter out that story
      array(0) = stories.filterNot { _.members.exists(s => c.members.contains(s)) }.size.toDouble / total
      // probability of happening
      // any sentence exists in that cluster => keep this story
      array(1) = stories.filter { _.members.exists(s => c.members.contains(s)) }.size.toDouble / total

      for (i <- 0 to 1)
        if (array(i) == 0) array(i) = INFINISMALL

      array
    }

  }
}