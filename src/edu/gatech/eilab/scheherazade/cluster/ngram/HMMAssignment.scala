package edu.gatech.eilab.scheherazade.cluster.ngram
import net.sf.javailp._
/**
 * solves ilp assignment problem for the dirichlet model
 *  @param probs: probs(i, j) is the log probability of sentence i belonging to cluster j
 *  @param transition: transition(i, j) is the log probability (freq) of cluster i followed by cluster j
 */
class HMMAssignment(probs: Array[Array[Double]], transition: Array[Array[Double]]) {

  def solve() = {
    //println("glpk")
    val numSents = probs.length
    val numClusters = probs(0).length
    val problem = new Problem();

    var objective = new Linear();
    for (i <- 0 until numSents; j <- 0 until numClusters; k <- 0 until numClusters) {
      val variable = "z" + i + "" +
        "c" + j + "c" + k
      objective.add(probs(i)(k) + transition(j)(k), variable)
      problem.setVarLowerBound(variable, 0);
      problem.setVarUpperBound(variable, 1);
      problem.setVarType(variable, classOf[Integer])
    }

    problem.setObjective(objective, OptType.MAX);

    // each sentence (and the subsequent sentence) can belong to only one cluster
    for (i <- 0 until numSents) {
      val constraint = new Linear()
      for (j <- 0 until numClusters; k <- 0 until numClusters) {
        val variable = "z" + i + "c" + j + "c" + k
        constraint.add(1, variable)
      }
      problem.add("1ClusterPerSent" + i, constraint, "=", 1)
    }

    // consistent transitions:
    // for all i, j, k, z_ijk + sum(k' != k, k'') z_i+1, k', k'' = 1
    /*for (i <- 0 until numSents-1; j <- 0 until numClusters; k <- 0 until numClusters) {
    	val constraint = new Linear()
    	val v1 = "z" + i + "c" + j + "c" + k
    	constraint.add(1, v1)
    	for (kp <- 0 until numClusters if kp != k; kpp <- 0 until numClusters)
    	{
    	  val v2 = "z" + (i+1) + "c" + kp + "c" + kpp
    	  constraint.add(1, v2)
    	}
    	
    	problem.add("consistent" + i + "c" + j + "c" + k, constraint, "<=", 1)
    }*/

    // MISTAKE: i really should start from 1. I don't know why that gives me an infeasible problem
    for (i <- 2 until numSents; j <- 0 until numClusters) {
      val constraint = new Linear()
      for (k <- 0 until numClusters) {
        val v1 = "z" + i + "c" + j + "c" + k
        constraint.add(1, v1)
      }
      
      for (jp <- 0 until numClusters) {
        val v2 = "z" + (i - 1) + "c" + jp + "c" + j
        constraint.add(-1, v2)
      }

      problem.add("consistent" + i + "c" + j, constraint, "=", 0)
    }

    // each cluster can contain only one sentence
    for (k <- 0 until numClusters) {
      val constraint = new Linear()
      for (i <- 0 until numSents; j <- 0 until numSents) {
        val variable = "z" + i + "c" + j + "c" + k
        constraint.add(1, variable)
      }
      problem.add("1SentPerCluster" + k, constraint, "<=", 1)
    }

    var solver = HMMAssignment.factory.get(); // you should use this solver only once for one problem
    var result = solver.solve(problem);

    //println(result)

    val answer = Array.ofDim[Int](numSents)
    for (i <- 0 until numSents) {
      for (j <- 1 until numClusters; k <- 1 until numClusters) {
        val variable = "z" + i + "c" + j + "c" + k

        if (result.get(variable).intValue() == 1) {
          answer(i) = k
        }
      }
    }

    answer
  }

}

object HMMAssignment {

  val factory = new SolverFactoryGLPK()
  factory.setParameter(Solver.TIMEOUT, 100000); // set timeout to 1000 seconds
  factory.setParameter(Solver.VERBOSE, 1);

}