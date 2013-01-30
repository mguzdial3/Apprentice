package edu.gatech.eilab.scheherazade.utils

import java.io._
import collection.mutable.ArrayBuffer
// converting the csv file generated by Mechanical Turk to our format
// the csv file should contain only the stories, not the metadata
object CSVProcessor {

  def readCSV(filename: String): Array[Array[String]] = {
    val text = scala.io.Source.fromFile(filename).mkString
    

    val lines = text.split("\n")
    val buffer = ArrayBuffer[Array[String]]()

    lines foreach {
      l =>
        println("line = " + l)
        val events = l.trim.replace(",,", ", ,").split(",")
        var next = 0
        var lineBuffer = ArrayBuffer[String]()

        while (next < events.length) {
          var entry = new StringBuilder() append events(next)
          //println("processing " + events(next))

          if (entry.startsWith("\"")) {

            // this entry is self contained
            if (entry.toString.replace("\"\"", "").endsWith("\"")) {
              entry.deleteCharAt(0)
              entry.deleteCharAt(entry.size-1)
              entry.replaceAllLiterally("\"\"", "")
              next += 1
            } else {
              var idx = next + 1
              entry = entry.deleteCharAt(0)

              while (!events(idx).endsWith("\"")) {
                entry append ("," + events(idx))
                //println("appending " + events(idx))
                idx += 1
              }

              val last = events(idx)
              entry append (", " + last.substring(0, last.length - 1))
              next = idx + 1
            }

          } else {
            next += 1 // the ordinary case. increment the counter
          }

          lineBuffer += entry.toString
        }
        //println("processed line = " + lineBuffer.mkString(", "))
        buffer += lineBuffer.toArray
    }

    buffer.toArray
   
  }
  
  def main(args:Array[String])
  {
    val array = readCSV("./data/coffee/data.csv")
    val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream("./data/coffee/coffeeStories.txt")))
    for(story <- array)
    {
      for(line <- story)
      {
        val k = line.trim
        if (k != "")
        {
        	writer.println(k)
        }
      }
      writer.println("###")
    }
    
    writer.close()
  }
}