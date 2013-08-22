package edu.gatech.eilab.scheherazade.cluster.ngram
import breeze.linalg._

object PCA {
  def pca(data: DenseMatrix[Double], components: Int) = {
    val d = zeroMean(data)
    val (_, _, v) = svd(d.t)
    val model = v(0 until components, ::) //top 'components' eigenvectors
    val filter = model.t * model
    filter * d
  }

  private def mean(v: Vector[Double]) = (v.valuesIterator.sum) / v.size

  private def zeroMean(m: DenseMatrix[Double]) = {
    val copy = m.copy
    for (c <- 0 until m.cols) {
      val col = copy(::, c)
      val colMean = mean(col)
      col -= colMean
    }
    // println("data \n" + m)
    // println("mean \n" + copy)
    copy
  }
}