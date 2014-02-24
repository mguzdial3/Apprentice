package edu.gatech.eilab.scheherazade.nlp

object NLPUtils {

  protected[nlp] def isUsefulPOS(pos: String): Boolean =
    {
      pos.startsWith("NN") || pos.startsWith("VB") || pos.startsWith("JJ") || pos.startsWith("RB")
    }
    
  /**
   * return the google POS in the form of "_NOUN" or "_VERB"
   *
   */
  protected[nlp] def toGooglePOS(pos: String): String =
    {
      if (pos.startsWith("N")) "_NOUN"
      else if (pos.startsWith("V")) "_VERB"
      else if (pos.startsWith("JJ")) "_ADJ"
      else if (pos.startsWith("RB")) "_ADV"
      else {
        System.err.println("WARNING: WHAT POS IS THIS ? " + pos)
        ""
      }
    }
}