package edu.gatech.eilab.scheherazade.cluster.ngram

import gurobi._
/**
 * solves ilp assignment problem for the dirichlet model
 *  @param probs: probs(i, j) is the log probability of sentence i belonging to cluster j
 *  @param transition: transition(i, j) is the log probability (freq) of cluster i followed by cluster j
 *
 *  Nov 26 2013: Modified to use Gurobi and corrected mistakes
 */
class HMMAssignment(probs: Array[Array[Double]], transition: Array[Array[Double]]) {

  def solve() = {
    val env = new GRBEnv("graph_qp.log")
    env.set(GRB.DoubleParam.TimeLimit, 2000)
    val model = new GRBModel(env)
    val numSents = probs.length
    val numClusters = probs(0).length

    // put all variables here
    val varMat = Array.ofDim[GRBVar](numSents, numClusters, numClusters)

    val objective = new GRBLinExpr()
    for (i <- 0 until numSents; j <- 0 until numClusters; k <- 0 until numClusters) {
      val varName = "z" + i + "" +
        "c" + j + "c" + k
      val variable = model.addVar(0.0, 1.0, 0.0, GRB.INTEGER, varName)
      objective.addTerm(probs(i)(k) + transition(j)(k), variable)
      varMat(i)(j)(k) = variable
    }
    model.update()

    model.setObjective(objective, GRB.MAXIMIZE);

    // The EXHAUSTIVE constraint: Each sentence (and the subsequent sentence) can belong to only one cluster
    for (i <- 0 until numSents) {
      val constraint = new GRBLinExpr()
      for (j <- 0 until numClusters; k <- 0 until numClusters) {
        constraint.addTerm(1, varMat(i)(j)(k))
      }
      model.addConstr(constraint, GRB.EQUAL, 1.0, "exhaustive" + i)
    }

    // consistent transitions:
    // for all i, j, k, z_ijk + sum(k' != k, k'') z_i+1, k', k'' = 1
    for (i <- 1 until numSents; j <- 0 until numClusters) {
      val constraint = new GRBLinExpr()
      for (k <- 0 until numClusters) {
        val v1 = varMat(i)(j)(k)
        constraint.addTerm(1, v1)
      }

      for (jp <- 0 until numClusters) {
        val v2 = varMat(i - 1)(jp)(j)
        constraint.addTerm(-1, v2)
      }

      model.addConstr(constraint, GRB.EQUAL, 0, "consistency" + i + "c" + j)
    }

    // UNIQUE MEMEBERSHIP constraint: each cluster can contain only one sentence
    for (k <- 0 until numClusters) {
      val constraint = new GRBLinExpr()
      for (i <- 0 until numSents; j <- 0 until numSents) {
        val variable = varMat(i)(j)(k)
        constraint.addTerm(1, variable)
      }
      model.addConstr(constraint, GRB.LESS_EQUAL, 1, "unique" + k)
    }

    // solve
    model.optimize

    //println(result)

    val answer = Array.ofDim[Int](numSents)
    for (i <- 0 until numSents) {
      for (j <- 1 until numClusters; k <- 1 until numClusters) {
        val variable = varMat(i)(j)(k)

        if (variable.get(GRB.DoubleAttr.X) > 0.99) {
          answer(i) = k
        }
      }
    }

    env.dispose()
    model.dispose()

    answer
  }

}

object HMMAssignment {


}