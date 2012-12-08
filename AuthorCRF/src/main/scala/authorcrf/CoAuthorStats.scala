package authorcrf

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import java.io._

object CoAuthorStats {

	val out = new BufferedWriter( new FileWriter( new File("coauth.feat") ) )

	val KeyStore = new HashMap[String, Node]()
	val LinkStore = new HashMap[String, Array[Link]]()
	var count = 0

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

	def generateCoauthorGraph(pair : Pair) : Double = {
    count += 1
		val graph = new Graph
		var search = ""
		if(KeyStore.contains(pair.publication1.pubkey)) {
			graph.add(KeyStore(pair.publication1.pubkey),LinkStore(pair.publication1.pubkey))
		} else {
			val coauthors1 = getCoAuthors(pair.publication1)
			graph.nodes(pair.publication1.block) = new Node(pair.publication1.block, null, true)
			for(co <- coauthors1) {
				if(!graph.nodes.contains(co)) {
					graph.nodes(co) = new Node(co, graph.nodes(pair.publication1.block))
				}

				//graph.nodes(pair.publication1.block).nodes(graph.nodes(co).author) = new Child(1,graph.nodes(co))
				//graph.nodes(pair.publication1.block).neigh(graph.nodes(co).author) = new Meta(1,graph.nodes(co))
				graph.addLink(Link(graph.nodes(pair.publication1.block), graph.nodes(co), 1), graph.nodes(pair.publication1.block).nickname)
				graph.nodes(co).rootParent = graph.nodes(pair.publication1.block)
				//graph.nodes(co).neigh(pair.publication1.block) = new Meta(1,graph.nodes(pair.publication1.block))
			}
      if(graph.links.contains(pair.publication1.block)) {
			  graph.addEndsFrom( graph.nodes(pair.publication1.block) )
      }
      		KeyStore(pair.publication1.pubkey) = graph.nodes(pair.publication1.block)
			LinkStore(pair.publication1.pubkey) = graph.links.values.flatten.toArray
		}
		if(KeyStore.contains(pair.publication2.pubkey)) {
			graph.add(KeyStore(pair.publication2.pubkey), LinkStore(pair.publication2.pubkey), (pair.publication2.block == pair.publication1.block))
      search = if(pair.publication1.block == pair.publication2.block) pair.publication2.block + "2" else pair.publication2.block
    } else {
			val coauthors2 = getCoAuthors(pair.publication2)
			if(pair.publication2.block != pair.publication1.block) graph.nodes(pair.publication2.block) = new Node(pair.publication2.block, null, true)
    		else {
    			graph.nodes(pair.publication2.block+"2") = new Node(pair.publication2.block, null, true)
				  graph.nodes(pair.publication2.block+"2").nick = (pair.publication2.block+"2")
			}
      search = if(pair.publication1.block == pair.publication2.block) pair.publication2.block + "2" else pair.publication2.block
      for(co <- coauthors2) {
				if(!graph.nodes.contains(co)) {
					graph.nodes(co) = new Node(co,graph.nodes(pair.publication2.block))
				}

				//graph.nodes(search).nodes(graph.nodes(co).author) = new Child(1,graph.nodes(co))
        if(pair.publication1.block == pair.publication2.block) { graph.addLink(Link(graph.nodes(pair.publication2.block + "2"), graph.nodes(co), 1), graph.nodes(search).nickname) }
        else { graph.addLink(Link(graph.nodes(pair.publication2.block), graph.nodes(co), 1), graph.nodes(search).nickname) }
				graph.nodes(co).rootParent = graph.nodes(search)
				//graph.nodes(co).parents(search) = graph.nodes(search)
			}
      if(graph.links.contains(search)) {
        if(pair.publication1.block == pair.publication2.block) { graph.addEndsFrom( graph.nodes(pair.publication2.block + "2") ) }
        else { graph.addEndsFrom( graph.nodes(pair.publication2.block) ) }
      }
      KeyStore(pair.publication2.pubkey) = graph.nodes(search)
			LinkStore(pair.publication2.pubkey) = graph.links.values.flatten.toArray
		}
		for(node <- graph.nodes) { node._2.color1 = 0; node._2.color2 = 0; node._2.fromColor = "" }
    	var d = (0,0)
		try {
		graph.color1(graph.nodes(pair.publication1.block), graph.nodes(search))
		graph.color2(graph.nodes(search), graph.nodes(pair.publication1.block))

	   	if(graph.links.contains(search) && graph.links.contains(pair.publication1.block)) {
    		d = graph.percentage(graph.nodes(search),graph.nodes(pair.publication1.block))
     		println("1-----------"); println(pair.publication1.pubkey + "\t" + pair.publication2.pubkey + "\t"); graph.printNeato(true)
     		val t = (pair.publication1.attr[ClusterId].target==pair.publication2.attr[ClusterId].target)
    		out.write(pair.publication1.pubkey + "\t" + pair.publication2.pubkey + "\t" + d + "\t" + t + "\n")
    		out.flush()
    	}
    	} catch {
    		case ne : java.util.NoSuchElementException => { println(ne.getMessage()); println(graph.nodes); println(pair.publication1.pubkey + "\t" + pair.publication2.pubkey) }
    	}

    /*if(count > 600) {
      println(pair.publication1)
      println(pair.publication2)
      if (KeyStore.contains(pair.publication1.pubkey)) {
        LinkStore(pair.publication1.pubkey)
      }
    } */
    	/*var d = (0,0)
    	if(graph.links.contains(search)) {
    		d = graph.percentage(graph.nodes(search))
     		if(d._1 >= 1) { println("1-----------"); println(pair.publication1.pubkey + "\t" + pair.publication2.pubkey + "\t"); graph.print() }
    		for(node <- graph.nodes) { node._2.color = 0; node._2.fromColor = "" }
			graph.color(graph.nodes(search), graph.nodes(pair.publication1.block))
    		if(graph.links.contains(pair.publication1.block)) {
    			val dnew = graph.percentage(graph.nodes(pair.publication1.block))
    			d = (d._1+dnew._1,d._2+dnew._2)
    		}
    		if(d._1 >= 1) { println("2-----------"); println(pair.publication1.pubkey + "\t" + pair.publication2.pubkey + "\t"); graph.print() }
    		val t = (pair.publication1.attr[ClusterId].target==pair.publication2.attr[ClusterId].target)
    		out.write(pair.publication1.pubkey + "\t" + pair.publication2.pubkey + "\t" + d + "\t" + t + "\n")
    		out.flush()
    	}*/
    	(d._1.toDouble/d._2.toDouble)
  }

	class Graph {
		val nodes = new HashMap[String, Node]()
		val links = new HashMap[String, Array[Link]]()

		def color1(node : Node, search : Node, from : Node = null, count : Int = 0) {
			node.color1 = 1
			if(from != null)
				node.fromColor = from.nickname
				if(count < 2 && node != search) {
				if(links.contains(node.nickname)) {
					for(link <- links(node.nickname)) { 
						val other = link.getOther(node)
						if(other != from) 
							color1(other, search, node, count+1)
					}
				}
			}
		}

		def color2(node : Node, search : Node, from : Node = null, count : Int = 0) {
			node.color2 = 1
			if(from != null)
				node.fromColor = from.nickname
			if(count < 2 && node != search) {
				if(links.contains(node.nickname)) {
					for(link <- links(node.nickname)) { 
						val other = link.getOther(node)
						if(other != from) 
							color2(other, search, node, count+1)
					}
				}
			}
		}


		def percentage(bot : Node, top : Node) : (Int, Int) = {
			var topp = 0
			var bottom = 0
			var colored = new HashMap[String, Boolean]()
			for(link <- links(bot.nickname)) {
				bottom += 1 
				val other = link.getOther(bot)
				if(links.contains(other.author)){
					for(link2 <- links(other.author)) {
						val other2 = link2.getOther(other)
						if(other2.color1==1 && other.color2==1) colored(other.author) = true
					}
				}
			}
			for(link <- links(top.nickname)) {
				topp += 1 
				val other = link.getOther(top)
				if(links.contains(other.author)){
					for(link2 <- links(other.author)) {
						val other2 = link2.getOther(other)
						if(other2.color1==1 && other.color2==1) colored(other.author) = true
					}
				}
			}
			val max = math.max(topp, bottom)
			val col = math.min(colored.size, max)
			(col,max)
		}

    def linkExists(link : Link): Boolean = {
      (links.contains(link.links.head.nickname) && links(link.links.head.nickname).filter{ x => x.links.contains(link.links.last) }.length > 0)
    }

		def addLink(link : Link, curNick : String = "") {
      if(!linkExists(link)) {
			  val n1 = link.links.head
			  val n2 = link.links.last
        if(n1.root) {
          if(links.contains(curNick)) links(curNick) = links(curNick) ++ Array(link)
          else links(curNick) = Array(link)
        } else {
          if(links.contains(n1.nickname)) links(n1.nickname) = links(n1.nickname) ++ Array(link)
			    else links(n1.nickname) = Array(link)
        }
        if(n2.root) {
         if(links.contains(curNick)) links(curNick) = links(curNick) ++ Array(link)
         else links(curNick) = Array(link)
        } else {
          if(links.contains(n2.nickname)) links(n2.nickname) = links(n2.nickname) ++ Array(link)
			    else links(n2.nickname) = Array(link)
        }
      }
		}

		def add(node : Node, linksv : Array[Link], nick : Boolean = false) {
			if(nick) node.nick = node.author + "2" else node.nick = ""
			nodes(node.nickname) = node
			for(link <- linksv) {
				if(link.links.contains(node)) addNode(node, link, linksv : Array[Link], node.nickname)
			}
		}

		def addNode(node : Node, link : Link, linksv : Array[Link], curNick : String, recurs : Boolean = true) {
      val other = link.getOther(node)
      if(nodes.contains(other.author)) {
				addLink(link.dup(node), curNick)
			} else {
				nodes(other.author) = other
				addLink(link.dup(node), curNick)
			}
			if(recurs) {
				for(link <- linksv) {
					if(link.links.contains(other))
						addNode(other, link, linksv, curNick, false)
				}
			}
		}

		def printNeato(all : Boolean = false) {
			println("graph G {")
			for(linka <- links) {
				for(link <- linka._2) {
					val from = link.links(0)
					val to = link.links(1)
					if(all || from.root || to.root || (from.color1==1 && from.color2==1 && from.rootParent == null) || (to.color1==1 && to.color2==1 && to.rootParent == null))
						println(from.nickname.replaceAll(" +","_") + "_" + from.color1 + "_" + from.color2 + " -- " + to.nickname.replaceAll(" +","_") + "_" + to.color1 + "_" + to.color2 + "[label=" + link.count + "];")
				}
			}
			println("}")
		}

		def print() {
			println("=========")
			for(node <- nodes) {
				if(node._2.root) println("ROOT")
				println(node._1 + " :-> " + links(node._1).map{ x => 
					val no = x.links.filter( _.author != node._2.author).head
					no.nickname + " (" + x.count + ")"
				}.mkString(" ;: ") + " | from: " + node._2.fromColor)
			}
		}


		def getCoauthors(node : Node) : Seq[(String,String,Int)] = {
			if(!node.author.contains("?")) UpdateDBLP.getCoauthors(node) else Array[(String,String,Int)]()
		}

    def changeCount(link : Link) {
       val lks = links(link.links.head.nickname).filter{ x => x.links.contains(link.links.last) }.head
       lks.count = link.count
    }

		def addParent(a : (String, String, Int), node : Node) {
			if(nodes.contains(a._1) && nodes(a._1).root && node.rootParent != null && nodes(a._1)==node.rootParent) {
				changeCount(Link(node.rootParent, node, a._3))
			} else {
				if(!nodes.contains(a._1)) {
					nodes(a._1) = new Node(a._1, node)
					nodes(a._1).urlpt = a._2
				}
				//node.nodes(a._1) = new Child(a._3,nodes(a._1))
				//nodes(a._1).parents(node.author) = node
				if(!(nodes(a._1).root) && (nodes(a._1).rootParent == null || nodes(a._1).rootParent != node.rootParent)) addLink( Link(nodes(a._1), node, a._3))
			}
		}

		def addEndsFrom(node : Node) {
			for(link <- links(node.nickname)) {
				val noded = link.getOther(node)
				if(!noded.root && noded.nodes.size == 0) {
					val coauthors = getCoauthors(noded)
					if(coauthors.length < 200)
						coauthors.foreach(addParent(_, noded))
				}
			}
		}
		/*def addEnds() {
			for(nodeKV <- nodes) {
				val node = nodeKV._2
				if(!node.root && node.nodes.size == 0) {
					val coauthors = getCoauthors(node)
					coauthors.foreach(addParent(_, node))
				}
			}
		}*/
	}

	class Child(var count : Int, val n : Node)
	class Link(val links : Array[Node], var count : Int= 1) {
		def getOther(node : Node) : Node = {
			links.filter( _.author != node.author ).head
		}

		def dup(node : Node) : Link = {
			Link(node, getOther(node), count)
		}
	}
	object Link{
		def apply(link1 : Node, link2 : Node, c : Int) : Link = {
			new Link(Array[Node](link1, link2), c)
		}
	}
	class Node(val author : String,val parent : Node = null, val root : Boolean = false) {
		var color1 = 0
		var color2 = 0
		var fromColor = ""
		var nick = ""
		def nickname : String = { if(nick.length == 0) author else nick }
		var urlpt = ""
		val parents = new HashMap[String, Node]()
		var rootParent : Node = null
		if(parent != null)
			parents(parent.author) = parent
		val nodes = new HashMap[String,Child]()
		override def toString : String = nickname
	}
} 
