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
