package edu.gatech.eilab.scheherazade.utils

import java.io._

/**
 * Generating ScalaDoc under Windows.
 *  This program relies on a windows command "dir", so it will not work under other OS.
 */
object DocMaker {

  def main(args: Array[String]) {
    
    Runtime.getRuntime().exec("cmd /C del ./scaladoc/*.* /s/q/f");

    val commandLine = new StringBuffer()
    commandLine append "D:/GreenSoft/scala/bin/scaladoc.bat -d ./scaladoc/ "

    var p = Runtime.getRuntime().exec("cmd /C dir src\\*.scala /b/s");
    var in = new BufferedReader(
      new InputStreamReader(p.getInputStream()));

    var line = in.readLine

    while (line != null) {
      //System.out.println(line);
      commandLine append line
      commandLine append " "

      line = in.readLine
    }

    commandLine append "-encoding Cp1252 -classpath "
    p = Runtime.getRuntime().exec("cmd /C dir library\\*.jar /b/s");
    in = new BufferedReader(
      new InputStreamReader(p.getInputStream()));

    line = in.readLine

    while (line != null) {
      //System.out.println(line);
      commandLine append line
      commandLine append " "

      line = in.readLine
    }

    //println(commandLine.toString)
    val command = commandLine.toString
    println(command)
    p = Runtime.getRuntime().exec(command)

    in = new BufferedReader(
      new InputStreamReader(p.getErrorStream()));

    line = in.readLine
    while (line != null) {      
      line = in.readLine
    }
    //    p.waitFor()
    //
    //    println("ready")
    //    readLine()
    //Thread.sleep(2000)
  }
}