package edu.gatech.eilab.scheherazade.utils.ilp

import net.sf.javailp._
import scala.math._

class ILPOptimizer {
var factory = new SolverFactoryGLPK(); // use lp_solve
  var problem = new Problem();
  var varName = "Z";
  var vS: Array[Array[Int]] = null;
  var vP: Array[Array[Double]] = null;

  /*
   * S: BOW of sentences, number of sentences * vocabulary size
   * P: cluster property, number of clusters * vocabulary size
   * Objective: \sum_i \sum_j \sum_k Z_{ik} log(P_{kj}) S_{ij}
   */
  def setParameters(S: Array[Array[Int]], P: Array[Array[Double]]) = {
    vS = S;
    vP = P;
    //factory.setParameter(Solver.VERBOSE, 0);
    factory.setParameter(Solver.TIMEOUT, 100000); // set timeout to 1000 seconds
    var objective = new Linear();
    for (i <- 0 until S.length) {
      for (j <- 0 until S(i).length) {
        for (k <- 0 until P.length) {
          objective.add(log(P(k)(j)) * S(i)(j), varName + i + k);
        }
      }
    }
    problem.setObjective(objective, OptType.MAX);
  }

  /*
  * Z: sentence cluster relationship, number of sentences * number of cluster, all integer {0, 1}
  * 
  * 
  * */
  def addConstraint(Z: Array[Array[Int]]) = {
    for (i <- 0 until Z.length) {
      var sumc = new Linear();
      for (j <- 0 until Z(i).length) {
        sumc.add(1, varName + i + j);
        if (Z(i)(j) == 1) {
//          var c1 = new Linear();
//          c1.add(1, varName + i + j);
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

  /**
   * return: Z:  number of sentences * number of clusters
   */
  def solve(): Array[Array[Double]] = {
    if (vS == null) {
      return null;
    }
    var numS = vS.length;
    var numC = vP.length;
    var solver = factory.get();
    var result = solver.solve(problem);
    var Z = Array.ofDim[Double](numS, numC);
    for (i <- 0 until numS) {
      for (j <- 0 until numC) {
        Z(i)(j) = result.get(varName + i + j).doubleValue();
      }
    }

    Z;
  }


}

object ILPOptimizer {

  def main(args: Array[String]) {

    val factory = new SolverFactoryLpSolve(); // use lp_solve
    factory.setParameter(Solver.VERBOSE, 0);
    factory.setParameter(Solver.TIMEOUT, 100); // set timeout to 100 seconds

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

    problem.setVarType("x", classOf[Integer]);
    problem.setVarType("y", classOf[Integer]);

    val solver = factory.get(); // you should use this solver only once for one problem
    val result = solver.solve(problem);

    println(result);

  }

}

