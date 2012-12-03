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

	def generateCoauthorGraph(pair : Pair) = {
		val coauthors1 = getCoAuthors(pair.publication1)
		val coauthors2 = getCoAuthors(pair.publication2)
		val graph = new Graph
		graph.nodes(pair.publication1.block) = new Node(pair.publication1.block, null, true)
		graph.nodes(pair.publication2.block) = new Node(pair.publication2.block, null, true)
		for(co <- coauthors1) {
			if(!nodes.contains(co)) {
				graph.nodes(co) = new Node(co, graph.nodes(pair.publication1.block))
			}
			graph.nodes(pair.publication1.block).nodes += new Child(1,graph.nodes(co))
			graph.nodes(co).parents(pair.publication1.block) = graph.nodes(pair.publication1.block)
		}
		for(co <- coauthors2) {
			if(!nodes.contains(co)) {
				graph.nodes(co) = new Node(co,graph.nodes(pair.publication2.block))
			}
			graph.nodes(pair.publication2.block).nodes += new Child(1,graph.nodes(co))
			graph.nodes(co).parents(pair.publication2.block) = graph.nodes(pair.publication2.block)
		}
		(0 until 3).foreach( graph.addEnds() )
	}

	class Graph {
		val nodes = new HashMap[String,Node]()

		def getCoauthors(node : Node) : Seq[String] = {
			if(!node.author.contains("?") UpdateDBLP.getCoauthors(node) else Array[String]()
		}

		def addParent(a : (String, String, Int), node : Node) {
			if(!nodes.contains(author)) {
				nodes(author) = new Node(a._1, node)
				nodes.urlpt = a._2
			}
			node.nodes(author) = new Child(a._3,nodes(a._1))
			nodes(author).parents(node.author) = node
		}

		def addEnds() {
			for(node <- nodes) {
				if(!node.root && node.nodes == null) {
					val coauthors = getCoauthors(node)
					coauthors.foreach(addParent(_, node))
				}
			}
		}
	}

	class Child(var count : Int, val n : Node)

	class Node(val author : String,val parent : Node = null, val root = false) {
		var urlpt = ""
		val parents = new HashMap[String, Node]()
		if(parent != null)
			parents(parent.author) = parent
		val nodes = new HashMap[String,Child]()
	}

} 