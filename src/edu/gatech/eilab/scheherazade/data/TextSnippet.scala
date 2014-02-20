package edu.gatech.eilab.scheherazade.data

import serialize.XStreamable

case class TextSnippet(override val id: Int, val sentences: List[Sentence]) extends SingleDescription(id) {

  override def allTokens() = sentences.flatMap(_.tokens.toList)
  
  override def equals(o: Any) = o match {
    case t: TextSnippet => this.id == t.id
    case s: Sentence => (this.sentences.size == 1) && (sentences.head seq s)
    case _ => false
  }

  def contains(s: Sentence) = sentences contains s

  override def toText() = sentences.map(_.toText).mkString(" ")

}

case class SnippetCluster(override val name: String,
  override val members: List[TextSnippet]) extends ClusterLike(name, members) with XStreamable[Cluster] {

}