package edu.gatech.eilab.scheherazade.utils.ilp

object ilptrials {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(94); val res$0 = 
  
  math.pow(math.E, 2.0);System.out.println("""res0: Double = """ + $show(res$0));$skip(19); val res$1 = 
  math.log(math.E);System.out.println("""res1: Double = """ + $show(res$1));$skip(15); val res$2 = 
  math.log(10);System.out.println("""res2: Double = """ + $show(res$2));$skip(110); 
  val co = math.log(0.03) + math.log (0.04) + math.log (0.001) + math.log (0.003) * 2 + math.log (0.0004) * 3;System.out.println("""co  : Double = """ + $show(co ));$skip(23); val res$3 = 
  math.pow(math.E, co);System.out.println("""res3: Double = """ + $show(res$3));$skip(61); 
  
  val a = 0.03*0.04*0.001*0.003*0.003*math.pow(0.0004, 3);System.out.println("""a  : Double = """ + $show(a ))}
  
  
}