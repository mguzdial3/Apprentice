package edu.gatech.eilab.scheherazade.utils.ilp

object ilptrials {
  
  math.pow(math.E, 2.0)                           //> res0: Double = 7.3890560989306495
  math.log(math.E)                                //> res1: Double = 1.0
  math.log(10)                                    //> res2: Double = 2.302585092994046
  val co = math.log(0.03) + math.log (0.04) + math.log (0.001) + math.log (0.003) * 2 + math.log (0.0004) * 3
                                                  //> co  : Double = -48.723613014367245
  math.pow(math.E, co)                            //> res3: Double = 6.912000000000053E-22
  
  val a = 0.03*0.04*0.001*0.003*0.003*math.pow(0.0004, 3)
                                                  //> a  : Double = 6.912000000000002E-22
  
  
}