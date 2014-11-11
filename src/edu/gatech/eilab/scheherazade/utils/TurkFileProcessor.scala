package edu.gatech.eilab.scheherazade.utils

import java.io._
import java.util.Properties
/**
 * automated processing of downloaded turk data
 * The limitation is that each field must not contain a new line character
 *
 */
object TurkFileProcessor {

  def main(args: Array[String]) {
    val pathString = """C:\Users\Albert\Desktop\Proposal"""
    val directory = new File(pathString)
    val dataFiles = directory.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) =
        {
          name.endsWith(".csv")
        }
    })

    val allRecords = dataFiles.flatMap(readCSVFile)
    
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(pathString + "/initial11.txt"))))
    for (record <- allRecords)
    {
      if (record.getProperty("AssignmentStatus") == "Approved")
      {
        printTo(pw, record)
      }
    }
    
    pw.close
    
  }
  
  def printTo(pw:PrintWriter, record:Properties)
  {    
    for (i <- 1 to 20)
    {
      val key = "Answer.Event" + i
      val value = record.getProperty(key).trim
      if (value != "")
      {
        pw.println(value)
      }
    }
    pw.println("###")
  }
  
  def readCSVFile(file:File):Array[Properties] = {
    println("reading file " + file.getName())
    val table = CSVProcessor2.readCSV(file)
    val headings = table(0)
    println("headings = " + headings.mkString(", "))
    val records = Array.ofDim[Properties](table.length - 1)
    for (i <- 1 until table.length)
    {
      val property = new Properties()
      //println("processing line " + i)
      for (j <- 0 until table(i).length)
      {
//        println(i)
//        println(j)
        println(table(i).mkString)
        property.setProperty(headings(j), table(i)(j))
        println(headings(j) + " = " + table(i)(j))
      }
      //property.list(System.out)
      records(i-1) = property
    }
    
    records
  }

}