package edu.gatech.eilab.scheherazade.utils.ilp

import net.sf.javailp._
import scala.math._
import edu.gatech.eilab.scheherazade.utils.Matrix

/**
 * sentVocab is a matrix show the words each sentence contains. Its dimension = number of sentences * number of words
 *  sentVocab is a matrix show the words each sentence contains. Its dimension = number of clusters * number of words
 */
class ILPProblem(val sentVocab: Array[Array[Int]], val clusterVocab: Array[Array[Int]], val orders: List[(Int, Int)]) {

  require(sentVocab(0).length == clusterVocab(0).length)
  protected var problem = new Problem();
  protected var varName = "Z";

  protected val numSents = sentVocab.length
  protected val numClusters = clusterVocab.length
  protected val numWords = sentVocab(0).length
  
  var verbose = false

  /**
   * solve the corresponding binary integer problem
   * @return Z:  number of sentences * number of clusters
   */
  def solve(): Array[Array[Double]] = {

    /* computing the probability of each word in each cluster
     * For each cluster, the probabilities are normalized so they sum to one
     */
    val clusterProb = Array.ofDim[Double](numClusters, numWords)
    for (i <- 0 until numClusters) {
      val total: Double = clusterVocab(i).sum
      for (j <- 0 until numWords) {
        clusterProb(i)(j) = clusterVocab(i)(j) / total + 1E-6 
        // add a small constant so it does not become zero
      }
    }

    //println("probabilities")
    //Matrix.prettyPrint(clusterProb)
    
    setParameters(sentVocab, clusterProb)

    addClusterOrder(orders)

    var solver = ILPProblem.factory.get();
    var result = solver.solve(problem);
    
    if (verbose) println(result)
    // return Z
    var Z = Array.ofDim[Double](numSents, numClusters);
    for (i <- 0 until numSents) {
      for (j <- 0 until numClusters) {
        Z(i)(j) = result.get(varName + i + j).doubleValue();
      }
    }

    Z;
  }

  /**
   * computes log (n!)
   *
   */
  protected def logFactorial(n: Int): Double =
    {
      var result = 0.0
      for (i <- 2 to n) result += log(i)

      result
    }

  /*
   * S: BOW of sentences, number of sentences * vocabulary size
   * P: cluster property, number of clusters * vocabulary size
   * Objective: \sum_i \sum_j \sum_k Z_{ik} log(P_{kj}) S_{ij}
   */
  protected def setParameters(S: Array[Array[Int]], P: Array[Array[Double]]) = {

    //factory.setParameter(Solver.VERBOSE, 0);

    val objective = new Linear()

    for (i <- 0 until numSents) {

      /* This is the first part of the multinomial distribution:
       * n! / (x1! * x2! * x3! ... xn!)
       * It is a constant for a given sentence
       */
      val wordCnt = S(i).sum
      var headCoeff = logFactorial(wordCnt)
      for (j <- 0 until numWords) {
        headCoeff -= logFactorial(sentVocab(i)(j))
      }

      /* for each cluster, we compute its an objective function
       * 
       */
      var sumc = new Linear();

      for (k <- 0 until numClusters) {
        var coeff = headCoeff
        for (j <- 0 until numWords) {
          println(log(P(k)(j)) * S(i)(j))
          coeff += log(P(k)(j)) * S(i)(j)
        }
        
        // the variables are named Z + (sentence index) + (cluster index)
        val payoff = pow(math.E, coeff)
        objective.add(payoff, varName + i + k)
        //println("payoff for " + i + " " + k + " = " + pow(math.E, coeff))

        // set ranges of variables 
        problem.setVarLowerBound(varName + i + k, 0);
        problem.setVarUpperBound(varName + i + k, 1);
        problem.setVarType(varName + i + k, classOf[Integer]);

        // sum of an entire row
        sumc.add(1, varName + i + k)
      }

      // a sentence belongs to at most one cluster
      problem.add(sumc, "<=", 1)

    }
    problem.setObjective(objective, OptType.MAX);

  }

  /**
   * add ordering between clusters, which implies certain mutex between the variables.
   *  The orderings are a pair of integers, indicating that the cluster denoted by the first integer
   *  precedes the cluster denoted by the 2nd integer
   *
   */
  def addClusterOrder(orders: List[(Int, Int)]) {
    for (pair <- orders) {
      val first = pair._1
      val second = pair._2
      
      for (i <- 1 until numClusters; j <- 0 until i) {
        val name1 = varName + i + pair._1
        val name2 = varName + j + pair._2
        //val name3 = varName + j + pair._1
        
        val mutex = new Linear()
        mutex.add(1, name1)
        mutex.add(1, name2)
        problem.add(mutex, "<=", 1)
        
        println("added mutex " + name1 + " " + name2)
      }
    }
  }

  /**
   * add mutex constraints between variables.
   *  @param a list of mutex constraints. Each constraint is expressed as four integers,
   *  since each variable is indexed by two integers.
   *
   */
  protected def addMutexConstraint(constraints: List[Array[Int]]) {
    for (c <- constraints) {
      val name1 = varName + c(0) + c(1)
      val name2 = varName + c(2) + c(3)

      val mutex = new Linear()
      mutex.add(1, name1)
      mutex.add(1, name2)
      problem.add(mutex, "<=", 1);
    }
  }

  /*
  * Z: sentence cluster relationship, number of sentences * number of cluster, all integer {0, 1}
  * 
  * 
  * */
  @deprecated
  def addConstraint(Z: Array[Array[Int]]) = {
    for (i <- 0 until Z.length) {
      var sumc = new Linear();
      for (j <- 0 until Z(i).length) {
        sumc.add(1, varName + i + j);
        if (Z(i)(j) == 1) {

          problem.setVarLowerBound(varName + i + j, 1);
          problem.setVarUpperBound(varName + i + j, 1);

          //          problem.add(c1, "=", 1);
        } else {

          //          var c1 = new Linear();
          //          c1.add(1, varName + i + j);
          //          problem.add(c1, "<=", 1);
          //          var c2 = new Linear();
          //          c2.add(1, varName + i + j);
          //          problem.add(c1, ">=", 0);
          problem.setVarLowerBound(varName + i + j, 0);
          problem.setVarUpperBound(varName + i + j, 1);
          problem.setVarType(varName + i + j, classOf[Integer]);
        }
      }
      problem.add(sumc, "=", 1);
    }
  }

}

object ILPProblem {

  val factory = new SolverFactoryGLPK()
  factory.setParameter(Solver.TIMEOUT, 100000); // set timeout to 1000 seconds

  def main(args: Array[String]) {
    testILPProblem()
  }
  
  /** testing the class ILPProblem for real
   *  
   */
  def testILPProblem() {
    val cVocab = Array.fill[Int](3, 3)(1)
    val sVocab = Array.fill[Int](3, 3)(0)
    cVocab(0)(0) = 3
    cVocab(1)(1) = 3
    cVocab(2)(2) = 3
    sVocab(0)(0) = 1
    sVocab(0)(1) = 4
    sVocab(0)(2) = 1
    sVocab(1)(0) = 5
    sVocab(2)(2) = 3
    val orders = List((0, 1), (1, 2))
    
    val p = new ILPProblem(sVocab, cVocab, orders)
    val z = p.solve
    println()
    println(z)
    println("hello")
    Matrix.prettyPrint(z)
    
  }

  /**
   *  The payoff matrix is
   *  4 5   0
   *  0 50 40
   *
   *  the second sentence must happen after the first sentence
   *
   */
  def testConstraints() {
    val problem = new Problem();

    var objective = new Linear();
    objective.add(4, "z11");
    objective.add(5, "z12");
    objective.add(0, "z13");

    objective.add(0, "z21");
    objective.add(50, "z22");
    objective.add(40, "z23");

    problem.setObjective(objective, OptType.MAX);

    val line1 = new Linear()
    line1.add(1, "z11")
    line1.add(1, "z12")
    line1.add(1, "z13")
    problem.add(line1, "<=", 1)

    val line2 = new Linear()
    line2.add(1, "z21")
    line2.add(1, "z22")
    line2.add(1, "z23")
    problem.add(line2, "<=", 1)

    val mutex1 = new Linear()
    mutex1.add(1, "z11")
    mutex1.add(1, "z21")
    problem.add(mutex1, "<=", 1)

    val mutex2 = new Linear()
    mutex2.add(1, "z12")
    mutex2.add(1, "z21")
    problem.add(mutex2, "<=", 1)

    val mutex3 = new Linear()
    mutex3.add(1, "z12")
    mutex3.add(1, "z22")
    problem.add(mutex3, "<=", 1)

    val mutex4 = new Linear()
    mutex4.add(1, "z13")
    mutex4.add(1, "z22")
    problem.add(mutex4, "<=", 1)

    val mutex5 = new Linear()
    mutex5.add(1, "z13")
    mutex5.add(1, "z23")
    problem.add(mutex5, "<=", 1)

    val mutex6 = new Linear()
    mutex6.add(1, "z13")
    mutex6.add(1, "z21")
    problem.add(mutex6, "<=", 1)

    for (i <- 1 to 2; j <- 1 to 3) {
      problem.setVarLowerBound("z" + i + j, 0);
      problem.setVarUpperBound("z" + i + j, 1);
      problem.setVarType("z" + i + j, classOf[Integer])
      problem.setVarType("z" + i + j, classOf[Integer])
    }

    var solver = factory.get(); // you should use this solver only once for one problem
    var result = solver.solve(problem);

    println(result)

    for (i <- 1 to 2) {
      for (j <- 1 to 3) {
        val r = result.get("z" + i + j).intValue()
        print(r + " ")
      }
      println
    }

  }

  def testILPEngine() {
    factory.setParameter(Solver.VERBOSE, 0);
    factory.setParameter(Solver.TIMEOUT, 100); // set timeout to 100 seconds

    /**
     * Constructing a Problem:
     * Maximize: 143x+60y
     * Subject to:
     * 120x+210y <= 15000
     * 110x+30y <= 4000
     * x+y <= 75
     *
     * With x,y being integers
     *
     */
    val problem = new Problem();

    var linear = new Linear();
    linear.add(143, "x");
    linear.add(60, "y");

    problem.setObjective(linear, OptType.MAX);

    linear = new Linear();
    linear.add(120, "x");
    linear.add(210, "y");

    problem.add(linear, "<=", 15000);

    linear = new Linear();
    linear.add(110, "x");
    linear.add(30, "y");

    problem.add(linear, "<=", 4000);

    linear = new Linear();
    linear.add(1, "x");
    linear.add(1, "y");

    problem.add(linear, "<=", 75);

    problem.setVarType("x", classOf[Integer])
    problem.setVarType("y", classOf[Integer]);

    var solver = factory.get(); // you should use this solver only once for one problem
    var result = solver.solve(problem);

    println(result);

    /**
     * Extend the problem with x <= 16 and solve it again
     */
    problem.setVarUpperBound("x", 16);

    solver = factory.get();
    result = solver.solve(problem);

    println(result);

    /*
     * Objective: 6266.0 {y=52, x=22}
	 * Objective: 5828.0 {y=59, x=16}
     */

  }

}

