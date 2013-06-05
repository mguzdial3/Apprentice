package edu.gatech.eilab.scheherazade

import java.io._
import edu.gatech.eilab.scheherazade.data.XStreamable
package data {
  class StorySet(
    val name: String,
    var storyList: List[Story]) extends XStreamable {
  }
}