package edu.gatech.eilab.scheherazade.graph.structure

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._

/**
 * This code finds independent units in plot graphs.
 *  An independent unit is like
 *     *
 *    /|\
 *   * * *
 *    \|/
 *     *
 *  If an independent unit does not have mutual exclusion with outside vertices, it can be treated as one unit that appears and disappears together from outside.
 */

object UnitAnalysis {

}

class UnitLabel(val source:Cluster, val target:Cluster, val split:Int, val inSplit:Int)