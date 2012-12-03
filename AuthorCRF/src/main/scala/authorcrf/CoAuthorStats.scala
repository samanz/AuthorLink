package authorcrf

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object CoAuthorStats {

	def addCoAuthorFeatures(pair : Pair) {
		val features = pair.attr[CoAuthorsFeatures]
		val graph = generateCoauthorGraph(pair)
	}

	def removeDigits(words : Seq[String]) : Seq[String] = {
		words.filter( x => !(x.matches("[0-9]*")) )
	}

	def getCoAuthors(pub : Publication) : Array[String] = {
		val coauthors = ArrayBuffer[String]()
		val bsp = pub.block.split(" ")
		for(author <- pub.coAuthors.coauthors) {
			val as = removeDigits(author.split(" "))
			if(as.head != bsp.head && as.last != bsp.last) { coauthors += author }
		}
		coauthors.toArray
	}

	def generateCoauthorGraph(pair : Pair) {
		val coauthors1 = getCoAuthors(pair.publication1)
		val coauthors2 = getCoAuthors(pair.publication2)
		val graph = new Graph
		graph.nodes(pair.publication1.block) = new Node(pair.publication1.block, null, true)
		graph.nodes(pair.publication2.block) = new Node(pair.publication2.block, null, true)
		for(co <- coauthors1) {
			if(!graph.nodes.contains(co)) {
				graph.nodes(co) = new Node(co, graph.nodes(pair.publication1.block))
			}
			graph.nodes(pair.publication1.block).nodes(graph.nodes(co).author) = new Child(1,graph.nodes(co))
			graph.nodes(co).parents(pair.publication1.block) = graph.nodes(pair.publication1.block)
		}
		for(co <- coauthors2) {
			if(!graph.nodes.contains(co)) {
				graph.nodes(co) = new Node(co,graph.nodes(pair.publication2.block))
			}
			graph.nodes(pair.publication2.block).nodes(graph.nodes(co).author) = new Child(1,graph.nodes(co))
			graph.nodes(co).parents(pair.publication2.block) = graph.nodes(pair.publication2.block)
		}
		for(i <- 0 until 3) graph.addEnds()
	}

	class Graph {
		val nodes = new HashMap[String,Node]()

		def getCoauthors(node : Node) : Seq[(String,String,Int)] = {
			if(!node.author.contains("?")) UpdateDBLP.getCoauthors(node) else Array[(String,String,Int)]()
		}

		def addParent(a : (String, String, Int), node : Node) {
			if(!nodes.contains(a._1)) {
				nodes(a._1) = new Node(a._1, node)
				nodes(a._1).urlpt = a._2
			}
			node.nodes(a._1) = new Child(a._3,nodes(a._1))
			nodes(a._1).parents(node.author) = node
		}

		def addEnds() {
			for(nodeKV <- nodes) {
				val node = nodeKV._2
				if(!node.root && node.nodes == null) {
					val coauthors = getCoauthors(node)
					coauthors.foreach(addParent(_, node))
				}
			}
		}
	}

	class Child(var count : Int, val n : Node)

	class Node(val author : String,val parent : Node = null, val root : Boolean = false) {
		var urlpt = ""
		val parents = new HashMap[String, Node]()
		if(parent != null)
			parents(parent.author) = parent
		val nodes = new HashMap[String,Child]()
	}

} 