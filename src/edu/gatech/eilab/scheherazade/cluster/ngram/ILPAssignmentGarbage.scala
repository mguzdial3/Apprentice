package edu.gatech.eilab.scheherazade.cluster.ngram
import net.sf.javailp._
/**
 * solves ilp assignment problem for the dirichlet model
 * has a garbage cluster, which is the last one
 *  @param probs: each row is the probabilities of a sentence belonging to different clusters
 *
 */
class ILPAssignmentGarbage(probs: Array[Array[Double]]) {

  def solve() = {
    val rows = probs.length
    val cols = probs(0).length
    val problem = new Problem();

    var objective = new Linear();
    for (i <- 0 until rows; j <- 0 until cols) {
      val variable = "z" + i + "c" + j      
      objective.add(probs(i)(j), variable)
      problem.setVarLowerBound(variable, 0);
      problem.setVarUpperBound(variable, 1);
      problem.setVarType(variable, classOf[Integer])
    }

    problem.setObjective(objective, OptType.MAX);

    // each sentence can belong to only one cluster
    for (i <- 0 until rows) {
      val constraint = new Linear()
      for (j <- 0 until cols) {
        val variable = "z" + i + "c" + j
        constraint.add(1, variable)
      }
      problem.add("rowCons"+i, constraint, "=", 1)
    }
    
    // each cluster can contain only one sentence
    // except for the garbage cluster
    for (j <- 0 until cols) {
      val constraint = new Linear()
      for (i <- 0 until rows) {
        val variable = "z" + i + "c" + j
        constraint.add(1, variable)
      }
      problem.add("colCons"+j, constraint, "<=", 1)
    }

    var solver = ILPAssignment.factory.get(); // you should use this solver only once for one problem
    var result = solver.solve(problem);

    //println(result)

    val answer = Array.ofDim[Int](rows)
    for (i <- 0 until rows) {           
      for (j <- 1 until cols) {
        val variable = "z" + i + "c" + j

        if (result.get(variable).intValue() == 1) {
          answer(i) = j          
        }
      }
    }

    answer
  }

}