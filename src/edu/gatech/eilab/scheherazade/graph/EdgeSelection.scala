package edu.gatech.eilab.scheherazade.graph

import gurobi._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

object EdgeIntegerProblem {

  def selectEdges(clusters: List[Cluster], edges: List[ObservedLink]): Graph =
    {
      val n = clusters.size
      val cNumber = new HashMap[Cluster, Int]() ++ clusters.zip(1 to n).toList

      val lambda = -0.1
      
      val env = new GRBEnv("graph_qp.log")
      env.set(GRB.DoubleParam.TimeLimit, 20)
      val model = new GRBModel(env)
      val objective = new GRBLinExpr()

      // for each cluster, produce a variable z[i]
      val names = (1 to n).map("z" + _).toArray
      val zeros = Array.fill(n)(0.0)
      val types = Array.fill(n)(GRB.INTEGER)
      val z = model.addVars(zeros, Array.fill[Double](n)(n), zeros, types, names)

      // regularizer
//      for(i <- 0 until n)
//      {
//    	  objective.addTerm(lambda, z(i))
//      }
      
      // for each link, produce a variable y_ij
      val y = Array.ofDim[GRBVar](n, n)
      for (link <- edges) {
        val i = cNumber(link.source)
        val j = cNumber(link.target)
        val yName = "y" + i + "t" +  j
        val yVar = model.addVar(0.0, 1.0, 0.0, GRB.INTEGER, yName) // add the variable
        //println("adding " + yName)
        y(i - 1)(j - 1) = yVar
        objective.addTerm(link.confidence, yVar) // add term to the objective
      }
      
      println("number of variables = " + edges.size)
      model.update()

      // set objective
      model.setObjective(objective, GRB.MAXIMIZE);

      for (link <- edges) {
        /* add one constraint for each edge */
        val i = cNumber(link.source)
        val j = cNumber(link.target)
        val yVar = y(i - 1)(j - 1)
        val cName = "c" + i + "t" + j
        val expr = new GRBQuadExpr()
        expr.addTerm(1.0, z(j - 1))
        expr.addTerm(-1.0, yVar, z(i - 1))
        model.addQConstr(expr, GRB.GREATER_EQUAL, 1.0, cName);
      }

      // solve
      model.optimize

      // retrieve results
      var goodEdges = ListBuffer[ObservedLink]()

      for (link <- edges) {
        val i = cNumber(link.source)
        val j = cNumber(link.target)
        val yVar = y(i - 1)(j - 1)
        val result = yVar.get(GRB.DoubleAttr.X)
        if (result > 0.5) {
          goodEdges += link
        }
      }

      env.dispose()
      model.dispose()
      
      goodEdges foreach{
        e => println(e.toString)
      }
      
      new Graph(clusters, goodEdges.toList)
    }

}