package authorcrf

import collection.mutable._

object VenueStatistics {

  def main(args: Array[String]) {
    val pubs = LoadDBLPCoref.fromFile(args(0))
    val venues = pubs.map(_.venue.string.trim.replaceAll("\\&amp;", "&").replaceAll(" \\([0-9]*\\)$", "")).toSet.toSeq

    val authorVenue = Map[String, Set[String]]() // venue -> set of authorid
    io.Source.fromFile(args(1)).getLines().foreach(l => {
      val fields = l.split("\t")
      val v = fields(1).replaceAll(" \\([0-9]*\\)$", "")
      if (venues.contains(v)) {
        val authors = authorVenue.getOrElseUpdate(v, Set())
        authors += fields(0)
      }
    })

    val notFound = Set[String]()
    val coVenues = Map[Set[String], Int]() // two venues -> number of authors
    for (i <- 0 to (venues.size - 2)) {
      for (j <- (i + 1) to (venues.size - 1)) {
        val v1 = venues(i)
        val v2 = venues(j)
        if (authorVenue.contains(v1) && authorVenue.contains(v2)) {
          val size = authorVenue(v1).intersect(authorVenue(v2)).size
          if (size > 0)
            coVenues.put(Set(v1, v2), size)
        } else {
          if (!authorVenue.contains(v1)) notFound += v1
          if (!authorVenue.contains(v2)) notFound += v2
        }
      }
    }

    println(notFound)

    coVenues.foreach(v => {
      println(v._1.mkString("\t") + "\t" + v._2)
    })

  }
}

object VenueStatThreshold {
  def similar(pub1: Publication, pub2: Publication): Boolean = {
    val w = pub1.block.split(" ")
    val w2 = pub2.block.split(" ")
    (w.head==w2.head && w.last==w2.last)
  }

  def trim(s: String) = s.trim().replaceAll("\\&amp;", "&").replaceAll(" \\([0-9]*\\)$", "")

  def main(args: Array[String]) {
    val venuePairStat = io.Source.fromFile("venuePair.count").getLines().map(l => {
      val f = l.split("\t")
      Set(f(0), f(1)) -> f(2).toInt
    }).toMap


    var lf = LoadDBLPCoref.fromFile(args(0))
    val pairs = new ArrayBuffer[Pair]()
    for (l1 <- 0 until lf.length) {
      val l = lf(l1)
      for (l2 <- (l1 + 1) until lf.length) {
        val k = lf(l2)
        if (similar(l, k)) {
          val p = new Pair(l, k)
          p.attr += new PubPairLabel(p, if (l.attr[ClusterId].target == k.attr[ClusterId].target) "YES" else "NO")
          pairs += p
        }
      }
    }

    val trueCounts = ArrayBuffer[Int]()
    val falseCounts = ArrayBuffer[Int]()
    pairs.foreach(p => {
      // if (p.publication1.venue.string.equals("Bioinformatics") && p.publication2.venue.string.equals("BMC Bioinformatics")) {
      // 	println("===========1 " + p.attr[PubPairLabel].categoryValue + " " + p.publication1.pubkey + " " + p.publication2.pubkey)
      // 	println(p.publication1.toString + "\n" + p.publication2.toString)
      // } else if (p.publication2.venue.string.equals("Bioinformatics") && p.publication1.venue.string.equals("BMC Bioinformatics")) {
      // 	println("===========2 " + p.attr[PubPairLabel].categoryValue + " " + p.publication1.pubkey + " " + p.publication2.pubkey)
      // 	println(p.publication1.toString + "\n" + p.publication2.toString)
      // }

      val truth = p.attr[PubPairLabel].targetCategory.equals("YES")
      val coVenueCount = venuePairStat.getOrElse(Set(trim(p.publication1.venue.string), trim(p.publication2.venue.string)), 0)
      val unknown = Set("BioVis", "VINCI", "UIC/ATC")
      if (coVenueCount == 0 && truth && !trim(p.publication1.venue.string).equals(trim(p.publication2.venue.string)) && !unknown.contains(p.publication1.venue.string) && !unknown.contains(p.publication2.venue.string)) {
	println("=====")
      	println(p.attr[PubPairLabel].categoryValue + " " + p.publication1.pubkey + " " + p.publication2.pubkey)
      	println(p.publication1.toString + "\n" + p.publication2.toString)
      }
      if (truth && !trim(p.publication1.venue.string).equals(trim(p.publication2.venue.string))) trueCounts += coVenueCount
      else falseCounts += coVenueCount
    })

    println("media true " + trueCounts.sorted.apply(trueCounts.size / 2) + ", " + (trueCounts.sum.toDouble / trueCounts.size))
    println("media false " + falseCounts.sorted.apply(falseCounts.size / 2) + ", " + (falseCounts.sum.toDouble / falseCounts.size))
    // println("true " + trueCounts.groupBy(c => c).mapValues(_.size).toSeq.sortBy(_._1).map(x => x._1 + " -> " + x._2).mkString("\n"))
    // println("false " + falseCounts.groupBy(c => c).mapValues(_.size).toSeq.sortBy(_._1).map(x => x._1 + " -> " + x._2).mkString("\n"))

  }
}
