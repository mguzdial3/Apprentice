package edu.gatech.eilab.scheherazade

import main._
import utils.CSVProcessor
import data._
import graph._
import io._
import java.io._
import graph.passage._
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import scala.util.Random


package generation {

  class InteractiveEngine {
    var graphIndex: GraphIndex = null
    var desc: List[TextDescription] = null
    var reader: ConfigReader = null
    var inputChoice: Int = 0
    var choiceSet:Boolean = false
    var walk: AbstractPassage = null
    var dataset:String= ""     
    //var humanChoices: List[Cluster]=null;
      
    var humanAuthor: Boolean = false;
    var randomAuthor: Boolean = false;

      
    def play(_dataset: String){
      dataset = _dataset;
	  	dataset match {
        case "movie" =>
          desc = readDescriptions("./data/new_movie/textDescriptions.csv")
          reader = new ConfigReader("configNewMvBest.txt")
        case "robbery" =>
          desc = readDescriptions("./data/robbery/textDescriptions.csv")
          reader = new ConfigReader("configRobBest.txt")
        case _ =>
          System.err.println("Unknown data set: " + dataset)
          System.exit(1)
      }
    	var (stories, clusters) = reader.initDataFiltered()

    	val para = reader.properties.allParameters()(0)

    	val minimumSize = para.intParam("minClusterSize")
    	val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
    	val insideStories = reader.filterUnused(stories, insideClusters)

    	val gen = new GraphGenerator(insideStories, insideClusters)
    	var graph: Graph = gen.generateQIP(para)("mutualExcl")._1

    	
    	
    	//Change the graph here to fix/make the graphs worse
    	if(humanAuthor && dataset=="robbery"){
    		graph = AddMutualExclusive(graph, "John opens bank door", "John drives to bank")
    	}
    	else if(humanAuthor && dataset=="movie"){
    		graph = Remove(graph, "John meets Sally")
    		graph = Remove(graph, "pick Sally up")
    		graph = Remove(graph, "buy drinks")
    		graph = AddMutualExclusive(graph, "buy refreshments", "buy popcorn")
    		graph = AddMutualExclusive(graph, "buy refreshments", "buy popcorn and soda")
    		graph = AddMutualExclusive(graph, "buy popcorn", "buy popcorn and soda")
    		graph = AddMutualExclusive(graph,"buy popcorn", "drink sodas")
    		graph = AddMutualExclusive(graph,"park car", "arrive at theater")
    		graph = AddMutualExclusive(graph,"leave theater", "walk to car")
    		graph = AddLink(graph,"show tickets", "enter theater" )
    		graph = AddLink(graph,"buy refreshments", "enter theater", "M" )
    		graph = AddLink(graph,"buy popcorn", "enter theater", "M" )
    		graph = AddLink(graph,"buy popcorn and soda", "enter theater", "M" )
    		graph = AddLink(graph,"enter theater", "find seats" )
    		graph = AddLink(graph,"drive to theater", "arrive at theater", "M" )
    		
    	}
    	else if(randomAuthor && dataset == "robbery"){
    		graph = MakeRandomBasedOnLength(graph, 18)
    		
    		
    		/**
    		for (i <- 0 until 6) {
    			var newNodes = graph.nodes
    			var randomIndex: Int = r.nextInt(newNodes.size)
    			graph = Remove(graph, newNodes(randomIndex).name)
    	
    		}
    		var i:Int = 0;
    		
    		while(i<6){
    		  var indexOne = r.nextInt(newNodes.size)
    		  var indexTwo = r.nextInt(newNodes.size)
    		  
    		  if(indexOne!=indexTwo){
    			  var norm: MutualExcl = new MutualExcl (newNodes(indexOne), newNodes(indexTwo))
    			  if(!graph.mutualExcls.contains(norm) ){
    				  i = i+1
    				  
    				  graph = AddMutualExclusive(graph, newNodes(indexOne).name,newNodes(indexTwo).name)
    			  }
    		  }
    		}
    		
    		
    		
    		for (i <- 0 until 1) {
    			val links: List[Link] = graph.links
    			var randomLink: Int = r.nextInt(links.size)
    			graph = RemoveLink(graph, links(randomLink).source.name, links(randomLink).target.name)
    	
    		}
    		* 
    		*/
    	  
    	}
    	else if(randomAuthor && dataset=="movie"){
    		graph = MakeRandomBasedOnLength(graph, 20)
    		
    		
    		
    		/**
    		 * 
    		 * var r: Random = new Random();
    		
    		
    		var i:Int = 0;
    		
    		while(i<6){
    		  var indexOne = r.nextInt(newNodes.size)
    		  var indexTwo = r.nextInt(newNodes.size)
    		  
    		  if(indexOne!=indexTwo){
    			  var norm: MutualExcl = new MutualExcl (newNodes(indexOne), newNodes(indexTwo))
    			  if(!graph.mutualExcls.contains(norm) ){
    				  i = i+1
    				  
    				  graph = AddMutualExclusive(graph, newNodes(indexOne).name,newNodes(indexTwo).name)
    			  }
    		  }
    		}
    		for (i <- 0 until 6) {
    			var newNodes = graph.nodes
    			var randomIndex: Int = r.nextInt(newNodes.size)
    			graph = Remove(graph, newNodes(randomIndex).name)
    	
    		}
    		
    		var r: Random = new Random();
    		
    		var i:Int = 0;
    		
    		while(i<6){
    		  var indexOne = r.nextInt(newNodes.size)
    		  var indexTwo = r.nextInt(newNodes.size)
    		  
    		  if(indexOne!=indexTwo){
    			  var norm: MutualExcl = new MutualExcl (newNodes(indexOne), newNodes(indexTwo))
    			  if(!graph.mutualExcls.contains(norm) ){
    				  i = i+1
    				  
    				  graph = AddMutualExclusive(graph, newNodes(indexOne).name,newNodes(indexTwo).name)
    			  }
    		  }
    		}
    		
    		
    		
    		for (i <- 0 until 6) {
    			val links: List[MutualExcl] = graph.mutualExcls
    			var randomLink: Int = r.nextInt(links.size)
    			graph = RemoveMutualExclusive(graph, links(randomLink))
    	
    		}
    		* 
    		*/
    		
    	}
    	
    	
    	walk = Passage.init(graph)
    }
    
    
    
    def MinDepth(graph:Graph): Int = {
      var depthList: List[Int]= Nil;

      var thisWalk:AbstractPassage = Passage.init(graph)
      var choices = thisWalk.fringe
      
      var walks: List[(AbstractPassage, Int)] = List((thisWalk,0))
      
      var tries:Int = 0;
      while(walks.size!=0 && tries<10){
        var i: Int = 0;
       	while(i<walks.size && i<300) {
       	  var thisWalk2= walks(i);
       	  walks= walks diff List(thisWalk2);
       	  
       	  if(thisWalk2._1.hasMoreSteps()){
       		  choices = thisWalk2._1.fringe
       		  for(j <-0 until choices.size){
       		    var thisWalk3 = thisWalk2._1.forward(choices(j))
       		    
       		   walks = walks.:::(List((thisWalk3,thisWalk2._2+1)))
       		}
       	  }
       	  else{
       	    var history = thisWalk2._1.history 
       	    history = history.reverse
       	    var strHistory:String = "";
       	    for(j <-0 until history.size){
       	    	strHistory= strHistory+" "+history(j).name
       	    }
       	    depthList = depthList.:::(List(thisWalk2._2))
       	  }
       	  
       	
       	  i=i+1;
       	}
       	tries=tries+1;
      }
      
      var depth:Int = 100

       for (i <-0 until depthList.size) {
    	   if(depthList(i)<depth){
    	     depth = depthList(i)
    	   }
       }
          
      return depth;
    }
    
    /**
    def WalkTheGraph(thisWalk:AbstractPassage, depthSoFar:Int) :Int={
      var choices = thisWalk.fringe
      
      var toReturn = depthSoFar
      
    	if(thisWalk.hasMoreSteps() && choices!=null){
    	 for (i <-0 until choices.size) {
    		 var chosen: Cluster= choices(i)
    		 var thisWalk2 = thisWalk.forward(chosen)
    		 toReturn = WalkTheGraph(thisWalk2,(depthSoFar+1)) 
    	 }
    	  toReturn    		
    	}
    	else{
    	  
    	  depthList = depthList.:::(List(depthSoFar))
    	  toReturn
    	}
    }
    */
    
    def MakeRandom(oldGraph:Graph) : Graph = {
    	val newNodes = oldGraph.nodes
    	val links: List[Link] = Nil
    	val mutex: List[MutualExcl] = Nil
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	var newGraph:Graph =new Graph(newNodes, links, mutex, op, con)
    	
    	val r:Random = new Random();
    	
    	var size: Int = 20;//I know it's 20//MinDepth(oldGraph)
    	
    	while(MinDepth(newGraph)<(size-2)){
    	var numLinks:Int = oldGraph.links.size-5+r.nextInt(11);
    	var numMutex:Int = oldGraph.mutualExcls.size-5+r.nextInt(11);
    	
    	if(numLinks<0){
    	  numLinks=0;
    	}
    	
    	if(numMutex<0){
    	  numMutex=0;
    	}
    	
    	var sourceList: List[Int] = List()
    	var targetList: List[Int] = List()
    	var pairsList: List[Pair] = List()
    		
    	while(sourceList.size<numLinks){
    	  var targetVal:Int = Random.nextInt(newNodes.size);
    	  var sourceVal: Int = Random.nextInt(newNodes.size);
    		  
    	  if(targetVal!=sourceVal && !sourceList.contains(targetVal)){
    	    var pair: Pair = new Pair(sourceVal, targetVal)
    	    
    	    if(!pairsList.contains(pair)){
    	    	targetList = targetList.:::(List(targetVal))
    	    	sourceList = sourceList.:::(List(sourceVal))
    	    	pairsList = pairsList.:::(List(pair))
    	    }
      	  }
    	}
    		
    	var mutexList: List[Pair] = List()
    		
    	while(mutexList.size<numMutex){
    		var oneVal:Int = Random.nextInt(newNodes.size);
    	  	var twoVal: Int = Random.nextInt(newNodes.size);
    		  
    	 	if(oneVal!=twoVal){
    	 		var pair: Pair = new Pair(oneVal, twoVal)
    	 		if(!mutexList.contains(pair)){
    	 			mutexList = mutexList.:::(List(pair))
    	 		}
    	 	}
    	}
    		
    	var str:String = "Links: ";
    	for (i <- 0 until sourceList.size) {
    		str = str+""+(sourceList(i))+" to "+(targetList(i))+". "
    		newGraph = AddLink(newGraph, newNodes(sourceList(i)).name,newNodes(targetList(i)).name)
    	}
    	
    	str = str+"\n"+"Mutex: "
    	for (i <- 0 until mutexList.size) {
    		str = str+""+(mutexList(i).one)+" to "+(mutexList(i).two)+". "
    		newGraph = AddMutualExclusive(newGraph, newNodes(mutexList(i).one).name,newNodes(mutexList(i).two).name)
    	}
    	
    	
    	}
    	
    	newGraph
    }
    
    def MakeRandomBasedOnLength(oldGraph:Graph, length: Int=20) : Graph = {
    	val newNodes = oldGraph.nodes
    	val links: List[Link] = Nil
    	val mutex: List[MutualExcl] = Nil
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	var newGraph:Graph =new Graph(newNodes, links, mutex, op, con)
    	
    	val r:Random = new Random();
    	
    	var numLinks:Int = oldGraph.links.size-5+r.nextInt(11);
    	var numMutex:Int = 1+r.nextInt(6);
    	
    	
    	var nodesToUse:List[Int] = Nil
    	
    	for (i <- 0 until newNodes.size) {
    		nodesToUse = nodesToUse.:::(List(i))
    	}
    	
    	if(numLinks<0){
    	  numLinks=0;
    	}
    	
    	if(numMutex<0){
    	  numMutex=0;
    	}
    	
    	var sourceList: List[Int] = List()
    	var targetList: List[Int] = List()
    	var pairsList: List[Pair] = List()
    	var origTemp: List[Int] = List()
    	
    	var currLength:Int = 0;
    	
    	while(currLength<length){
    	  var targetVal:Int = Random.nextInt(newNodes.size);
    	  
    	  var sourceVal: Int = Random.nextInt(newNodes.size);
    	  
    	  if(targetList.size!=0){
    		 sourceVal= targetList(0)
    	  }
    		  
    	  if(targetVal!=sourceVal && !sourceList.contains(targetVal)){
    	    var pair: Pair = new Pair(sourceVal, targetVal)
    	    	targetList = targetList.:::(List(targetVal))
    	    	sourceList = sourceList.:::(List(sourceVal))
    	    	
    	    	origTemp = origTemp.:::(List(sourceVal))
    	    	origTemp = origTemp.:::(List(targetVal))
    	    	
    	    	pairsList = pairsList.:::(List(pair))
    	    	currLength=currLength+1;
    	    	nodesToUse = nodesToUse diff List(targetVal)
    	    	nodesToUse = nodesToUse diff List(sourceVal)
    	    	
      	  }    	 
    	}
    	
    	while(nodesToUse.size>0){
    	  var nextToAdd: Int = nodesToUse(r.nextInt(nodesToUse.size))
    	  //println("Next To Add: "+nextToAdd)
    	  //Act as source
    	 // if(r.nextInt(2)==0){
    		var targetIndex:Int = r.nextInt(origTemp.size)
    	    var target:Int = origTemp(targetIndex)
    	    
    	    targetList = targetList.:::(List(target))
    	    sourceList = sourceList.:::(List(nextToAdd))
    	    
    	    nodesToUse = nodesToUse diff List(nextToAdd)
    		println("Target: "+target+newNodes(target).name +". Next To Add: "+nextToAdd+newNodes(nextToAdd).name )
    	  
    		if(targetIndex<origTemp.size-2){
    			var newIndex:Int = targetIndex+1+r.nextInt((origTemp.size-1)-(targetIndex+1))
    			var newSource:Int = origTemp(newIndex)
    			
    			if(origTemp(newIndex)!=origTemp(targetIndex)){
    				targetList = targetList.:::(List(nextToAdd))
    				sourceList = sourceList.:::(List(newSource))
    			}
    		}
    		else if(targetIndex==origTemp.size-2){
    		  var newSource:Int = origTemp(origTemp.size-1)
    		  targetList = targetList.:::(List(nextToAdd))
    		  sourceList = sourceList.:::(List(newSource))
    		}
    		
    	 // }
    	  /**
    	  else{
    	    var sourceIndex:Int=r.nextInt(origTemp.size)
    	    var source:Int = origTemp(sourceIndex)
    	    
    	    if(sourceIndex!=0){
    	    	targetList = targetList.:::(List(nextToAdd))
    	    	sourceList = sourceList.:::(List(source))
    	    
    	    	var newTarget:Int = r.nextInt(sourceIndex)
    	    
    	    	targetList = targetList.:::(List(newTarget))
    	    	sourceList = sourceList.:::(List(nextToAdd))
    	    
    	    	nodesToUse = nodesToUse diff List(nextToAdd)
    	    }
    	  }
    	  * 
    	  */
    	}
    	
    		
    	var mutexList: List[Pair] = List()
    	
    	while(mutexList.size<numMutex){
    		var oneVal:Int = Random.nextInt(newNodes.size);
    	  	var twoVal: Int = Random.nextInt(newNodes.size);
    		  
    	 	if(oneVal!=twoVal){
    	 		var pair: Pair = new Pair(oneVal, twoVal)
    	 		if(!mutexList.contains(pair)){
    	 			mutexList = mutexList.:::(List(pair))
    	 		}
    	 	}
    	}
    	
    	
    		
    	var str:String = "Source: ";
    	var str2:String = "Targets: ";
    	for (i <- 0 until sourceList.size) {
    		str = str+", "+(sourceList(i))
    		str2 = str2+", "+(targetList(i))
    		newGraph = AddLink(newGraph, newNodes(sourceList(i)).name,newNodes(targetList(i)).name)
    	}
    	str = str+"\n"+str2
    	
    	println("Links: "+str)
    	
    	str = "One: "
    	str2 = "Two: "
    	for (i <- 0 until mutexList.size) {
    		str = str+""+(mutexList(i).one)+", "
    		str2 = str2+""+(mutexList(i).two)+", "
    		newGraph = AddMutualExclusive(newGraph, newNodes(mutexList(i).one).name,newNodes(mutexList(i).two).name)
    	}
    	str = str+"\n"+str2
    	println("Mutex: "+str);
    	
    	
    	newGraph
    }
    
    
    def MakeRandomRobbery(oldGraph:Graph) :Graph ={
    	val newNodes = oldGraph.nodes
    	val links: List[Link] = Nil
    	val mutex: List[MutualExcl] = Nil
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	var newGraph:Graph =new Graph(newNodes, links, mutex, op, con)
    	//27 to 5. 18 to 9. 3 to 11. 33 to 9. 18 to 6. 13 to 9. 24 to 21. 26 to 4. 24 to 9.                       1 to 23. 19 to 13. 31 to 21. 26 to 20.  15 to 16. 33 to 29. 28 to 6. 18 to 31. 18 to 5. 
    	var intList:List[Int] = List (32, 12, 25, 18, 10, 18, 26, 31, 1, 25, 9, 27, 18, 3, 33, 18,  13, 24, 26, 24, 1, 30, 24, 1,  20, 19, 31, 26, 29, 3,  10, 31, 12, 15, 33, 28,18,18)
    	var intList2:List[Int] = List(21, 21, 2, 8,    4,2, 2,    32, 23, 21, 4, 5, 9, 11, 9, 6, 9,     21, 4, 9, 23,  17, 13, 16, 4, 13, 21, 20, 21, 2, 22, 19, 11, 16, 29, 6,31,5)
    	for (i <- 0 until intList.size) {
    	  if(intList(i) != intList2(i)){
    		  newGraph = AddLink(newGraph, newNodes(intList(i)).name,newNodes(intList2(i)).name)
    	  }
    	  else{
    	    System.out.println("Problem at: "+i)
    	  }
    	}
    	//7 to 10. 6 to 10. 16 to 27. 12 to 11. 8 to 21. 12 to 5. 28 to 24. 28 to 6. 7 to 33. 4 to 28. 10 to 1. 14 to 4. 24 to 23. 8 to 20. 13 to 21. 19 to 9. 
    	var intListb:List[Int] = List (7,6,16,12,8,   12,28,28,7,4,10,14,24,8,13,19)
    	var intListb2:List[Int] = List(10,10,27,11,21,5, 24,6,33,28,1,4,23,20,21,9)
    	
    	
    	for (i <- 0 until intListb.size) {
    	  if(intListb(i) != intListb2(i)){
    		 newGraph = AddMutualExclusive(newGraph, newNodes(intListb(i)).name,newNodes(intListb2(i)).name )
    	  }
    	  else{
    	    System.out.println("Problem at: "+i)
    	  }
    	}
    	
    	newGraph
    }
    
    def MakeRandomRobbery2(oldGraph:Graph) :Graph ={
    	val newNodes = oldGraph.nodes
    	val links: List[Link] = oldGraph.links
    	val links2: List[Link] = Nil
    	val mutex: List[MutualExcl] = oldGraph.mutualExcls
    	val mutex2: List[MutualExcl] = Nil
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	var newGraph:Graph =new Graph(newNodes, links2, mutex2, op, con)
    	
    	var intList:List[Int] = List(22, 26, 25, 1, 30, 3,7, 10, 14, 3, 24, 11, 20, 23, 29, 4, 4, 1, 23, 24, 30, 17, 17, 5, 33, 30, 31, 10, 26, 5, 4, 17, 5, 22, 32, 11, 6, 33, 16, 19, 32, 11, 24)
    	
    	for (i <- 0 until links.size) {
    	  newGraph = AddLink(newGraph, links(i).source.name,newNodes(intList(i)).name)
    	}
    	
    	var intListb:List[Int] = List (2)
    	
    	for (i <- 0 until intListb.size) {
    	  //newGraph = AddMutualExclusive(newGraph, mutex(i).c1.name,newNodes(intList(i)).name)
    	}
    	
    	newGraph
    }
    
    def MakeRandomMovie(oldGraph:Graph) :Graph ={
    	val newNodes = oldGraph.nodes
    	val links: List[Link] = Nil
    	val mutex: List[MutualExcl] =Nil
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	var newGraph:Graph =new Graph(newNodes, links, mutex, op, con)
    	
    	var intList:List[Int] = List (30, 32, 10, 31, 7, 22, 12, 11, 23, 16, 29, 14, 17, 25, 1, 30, 8, 13, 14, 3, 10, 30, 4, 18, 24, 13, 16, 27, 15, 20, 2, 31, 14, 30, 7, 22, 4, 9, 15, 3, 20)
    	var intList2:List[Int] = List(14, 26, 32, 8, 5, 10, 21, 10, 7, 27, 16, 6, 21, 24, 26, 10, 28, 8, 11, 1, 31, 31, 11, 27, 15, 32, 15, 26, 23, 3, 4, 27, 4, 32, 22, 31, 32, 17, 4, 0, 22)
    	for (i <- 0 until intList.size) {
    	  if(intList(i) != intList2(i)){
    		  newGraph = AddLink(newGraph, newNodes(intList(i)).name,newNodes(intList2(i)).name)
    	  }
    	  else{
    	    System.out.println("Problem at: "+i)
    	  }
    	}
    	
    	
    	var intListb:List[Int] = List (2)
    	var intListb2:List[Int] = List(4)
    	
    	for (i <- 0 until intListb.size) {
    	  if(intListb(i) != intListb2(i)){
    		  newGraph = AddMutualExclusive(newGraph, newNodes(intListb(i)).name,newNodes(intListb2(i)).name )
    	  }
    	  else{
    	    System.out.println("Problem at: "+i)
    	  }
    	}
    	
    	newGraph
    }
    
    def MakeRandomMovie2(oldGraph:Graph) :Graph ={
    	val newNodes = oldGraph.nodes
    	val links: List[Link] =oldGraph.links
    	val links2: List[Link] = Nil
    	val mutex: List[MutualExcl] =Nil
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	var newGraph:Graph =new Graph(newNodes, links2, mutex, op, con)
    	
    	var intList:List[Int] = List(14, 26, 32, 8, 5, 10, 21, 10, 7, 27, 16, 6, 21, 24, 26, 10, 28, 8, 11, 22, 1, 31, 31, 11, 27, 15, 32, 15, 26, 23, 3, 4, 27, 4, 32, 22, 31, 32, 17, 4, 0, 22)
    	for (i <- 0 until links.size) {
    		newGraph = AddLink(newGraph, links(i).source.name,newNodes(intList(i)).name)
    	}
    	
    	newGraph
    }
    
    
    def Remove(oldgraph:Graph, nameOfNode: String):Graph = {
    	val newNodes = oldgraph.nodes.filterNot(_.name == nameOfNode)
    	val links = oldgraph.links.filterNot(x => x.source.name == nameOfNode || x.target.name == nameOfNode)
    	val mutex = oldgraph.mutualExcls.filterNot(x => x.c1.name == nameOfNode || x.c2.name == nameOfNode)
    	val op = oldgraph.optionals.filterNot(_.name == nameOfNode)
    	val con = oldgraph.optionals.filterNot(_.name == nameOfNode)

    	new Graph(newNodes, links, mutex, op, con)
    }
    
    def AddMutualExclusive(oldGraph:Graph, nameOfNode1: String, nameOfNode2:String):Graph = {
    	val newNodes = oldGraph.nodes
    	
    	val node1List = newNodes.filter(_.name==nameOfNode1)
    	val node2List = newNodes.filter(_.name==nameOfNode2)
    	
    	val node1 = node1List(0)
    	val node2 = node2List(0)
    	
    	val links = oldGraph.links
    	val mutex = oldGraph.mutualExcls
    	
    	
    	val mutex2 = mutex.:::(List(new MutualExcl(node1,node2)))
    	
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	new Graph(newNodes, links, mutex2, op, con)
    }
    
    def RemoveMutualExclusive(oldGraph: Graph, m: MutualExcl): Graph = {
    	val links = oldGraph.links
    	val mutex = oldGraph.mutualExcls
    	
    	if(mutex.contains(m)){
    	  println("def contains")
    	}
    	
    	val mutex2 = mutex diff List(m)
    	
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	
    	new Graph(oldGraph.nodes, links, mutex2, op, con)
    }
    
    def AddLink(oldGraph:Graph, nameOfNode1: String, nameOfNode2:String, listType:String = "R"):Graph = {
    	val newNodes = oldGraph.nodes
    	
    	val node1List = newNodes.filter(_.name==nameOfNode1)
    	val node2List = newNodes.filter(_.name==nameOfNode2)
    	
    	val node1 = node1List(0)
    	val node2 = node2List(0)
    	
    	val links = oldGraph.links
    	val mutex = oldGraph.mutualExcls
    	
    	val links2 = links.:::(List(new Link(node1,node2,listType)))
    	
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	new Graph(newNodes, links2, mutex, op, con)
    }
    
    def RemoveLink(oldGraph:Graph, nameOfNode1: String, nameOfNode2:String, listType:String = "R"):Graph = {
    	val newNodes = oldGraph.nodes
    	
    	val node1List = newNodes.filter(_.name==nameOfNode1)
    	val node2List = newNodes.filter(_.name==nameOfNode2)
    	
    	val node1 = node1List(0)
    	val node2 = node2List(0)
    	
    	val links = oldGraph.links
    	val mutex = oldGraph.mutualExcls
    	
    	val links2 = links diff (List(new Link(node1,node2,listType)))
    	
    	val op = oldGraph.optionals
    	val con = oldGraph.optionals
    	
    	new Graph(newNodes, links2, mutex, op, con)
    }
    
    
    def GetDescription():String = {
    	var toReturn: String = "";
    	var actor = "John"
    	  
    	dataset match {
        case "movie" =>
          toReturn+="This is a story about a movie date*";
        case "robbery" =>
          toReturn+="This is a story about a robbery*";
        case _ =>
          System.err.println("Unknown data set: " + dataset)
      }
    	
    	val descripts = walk.fringe.map { c =>
    			val o = desc.find(_.name == c.name)
    			if (o.isEmpty) {
    				throw new RuntimeException(c.name + " is missing from the descriptions")
    				//(c, c.name)
    			}
    			val d = o.get
    			(c, d)
    	}
    	
    	val humanChoices = descripts.filter(pair => pair._2.actor == actor)
    	
    	if (humanChoices.size > 0) {
      			for (i <- 0 until humanChoices.length) {
      				val optionText = humanChoices(i)._2.optionText
      						toReturn+="-"+optionText+"*"
      			}
      		}
    	System.out.println("To Return: "+toReturn);
    	toReturn;
    }
    
     def execute(choice: Int): String ={
    	var step: Cluster = null
    	val actors = desc.map(_.actor).distinct
        var actor = "John"
          
       
        var fringe = walk.fringe
        var myChoice:(Cluster, TextDescription)  = makeChoice(choice, fringe, desc, actor);
        
    	var toReturn: String = "";
    	if(myChoice!=null){
    		step = myChoice._1 
    		walk = walk.forward(step)
        
    		if(choice==(-1))
    			toReturn+=myChoice._2.otherExec+"*"
    		else
    			toReturn+=myChoice._2.SelfExec+"*"
         
    		val descripts = walk.fringe.map { c =>
    			val o = desc.find(_.name == c.name)
    			if (o.isEmpty) {
    				throw new RuntimeException(c.name + " is missing from the descriptions")
    				//(c, c.name)
    			}
    			val d = o.get
    			(c, d)
    		}
        
    		val humanChoices = descripts.filter(pair => pair._2.actor == actor)
    		val npcChoices = descripts filterNot (humanChoices contains)
      
      		if (humanChoices.size > 0) {
      			for (i <- 0 until humanChoices.length) {
      				val optionText = humanChoices(i)._2.optionText
      						toReturn+="-"+optionText+"*"
      			}
      		}
    		
    		if(!walk.hasMoreSteps){
    			toReturn+="HARDSTOP";
    		}
    	}
          
        toReturn
    }
     
     def makeChoice(choice: Int, choices: List[Cluster], desc: List[TextDescription], actor: String): (Cluster, TextDescription) = {
      val descripts = choices.map { c =>
        val o = desc.find(_.name == c.name)
        if (o.isEmpty) {
          throw new RuntimeException(c.name + " is missing from the descriptions")
          //(c, c.name)
        }
        val d = o.get
        (c, d)
      }

      val humanChoices = descripts.filter(pair => pair._2.actor == actor)
      val npcChoices = descripts filterNot (humanChoices contains)
      var chosen: (Cluster, TextDescription) = null
      var madeByHuman = true

          if (choice == -1) {
        	//IF WE HAVE NO READ IN TEXT, THEN DO NPC ACTION
            if (npcChoices.length > 0) {
              // select a npc choice            
              val idx = math.floor(math.random * npcChoices.length).toInt
              chosen = npcChoices(idx)
              madeByHuman = false
            }
          } else {

            if (humanChoices.size == 0) throw new RuntimeException("No choices left. That's weird.")

            val idx = choice
            if (idx < 0 || idx >= humanChoices.length)
              println("Invalid Choice. Choose from 1 to " + humanChoices.length)
            else {
              chosen = humanChoices(idx)
            }
          }      

      if(chosen!=null){
    	val result = if (madeByHuman) chosen._2.SelfExec else chosen._2.otherExec
    	if (result == "None") println("problematic: " + chosen._2)
      }
      chosen
    }
    
    /**
    def execute(walk: AbstractPassage, desc: List[TextDescription]) {
      var playAgain = false
      //println("\n\n")
      do {
        var stroll = walk
        var step: Cluster = null
        val actors = desc.map(_.actor).distinct
        var actor = "John"
       
        do {
          var fringe = stroll.fringe
          step = makeChoice(fringe, desc, actor)
          stroll = stroll.forward(step)
        } while (stroll.hasMoreSteps)

        //println("The End.\n\n\nLet's play again!\n\n")
        var input: Char = 0

      } while (playAgain)
      //println("Thank you for playing the game! \n Copyright 2012-2014 Entertainment Intelligence Lab, Georgia Tech.")
    }
    * 
    */

     /**
    def makeChoice(choices: List[Cluster], desc: List[TextDescription], actor: String): Cluster = {

      val descripts = choices.map { c =>
        val o = desc.find(_.name == c.name)
        if (o.isEmpty) {
          throw new RuntimeException(c.name + " is missing from the descriptions")
          //(c, c.name)
        }
        val d = o.get
        (c, d)
      }

      val humanChoices = descripts.filter(pair => pair._2.actor == actor)
      val npcChoices = descripts filterNot (humanChoices contains)
      var chosen: (Cluster, TextDescription) = null
      var madeByHuman = true

      while (chosen == null) { // repeat until a choice is made

        var readText = ""

        if (humanChoices.size > 0) {
          //println("Now you have the following choices: ")
          for (i <- 0 until humanChoices.length) {
            val optionText = humanChoices(i)._2.optionText
            //println((i + 1) + ". " + optionText)
          }
          //print("Your choice is: ")

          if (npcChoices.size > 0) {

            // can make a npc choice. apply a timeout on input
            var sleep = 0
            while (readText == "" && sleep < 10) {
              if (System.in.available() > 0) {
                val char = Array.ofDim[Char](3)
                var i = 0
                while (System.in.available() > 0 && i < 3) {
                  char(i) = System.in.read().asInstanceOf[Char]
                  i += 1
                }
                readText = new String(char).trim
                // guarding against empty input
                if (readText.length > 0)
                  readText = readText.substring(0, 1)

              } else {
                sleep += 1
                Thread.sleep(500) // sleep for 0.5 sec
              }
            }

            if (readText == "") println() // this line break comes after "Your choice is:"

          } else {
            readText = readLine()
          }
        }

        try {
          if (readText == "") {
        	//IF WE HAVE NO READ IN TEXT, THEN DO NPC ACTIOn
            if (npcChoices.length > 0) {
              // select a npc choice            
              val idx = math.floor(math.random * npcChoices.length).toInt
              chosen = npcChoices(idx)
              madeByHuman = false
            }
          } else {

            if (humanChoices.size == 0) throw new RuntimeException("No choices left. That's weird.")

            val idx = readText.toInt - 1
            if (idx < 0 || idx >= humanChoices.length)
              println("Invalid Choice. Choose from 1 to " + humanChoices.length)
            else {
              chosen = humanChoices(idx)
            }
          }
        } catch {
          // catch the number format exception that may happen when converting a string to a number
          // we do not need to do anything there because the choice is just empty if there is an exception
          case numEx: NumberFormatException =>
        }
      }

      val result = if (madeByHuman) chosen._2.SelfExec else chosen._2.otherExec
      if (result == "None") println("problematic: " + chosen._2)
      println()
      println(result)
      println()
      chosen._1
    }
    * */

    def readDescriptions(filename: String): List[TextDescription] = {
      val lines = CSVProcessor.readCSV(filename)
      val answer = for (row <- lines) yield {
        
        new TextDescription(row(0), row(1), row(2), row(3), row(4))
      }
      answer.toList.tail
    }
  }

  case class TextDescription(val name: String, val actor: String, val optionText: String, val SelfExec: String, val otherExec: String)

  class Pair(val one: Int, val two: Int) {  

  override def toString() = one + " - " + one
  
  //def graphvisString = source.name.replace(" ", "_") + " -> " + target.name.replace(" ", "_")
  
  override def equals(o: Any) = o match {
    case other: Pair => ((this.one == other.one && this.two == other.two) || (this.one == other.two && this.two == other.one))
    case _ => false
  }
  override def hashCode(): Int = (one.hashCode() + two.hashCode()) * 19 / 97  
}
}