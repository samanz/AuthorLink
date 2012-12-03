package authorcrf

import collection.mutable._

object VenueStatistics {

  def main(args: Array[String]) {
    val pubs = LoadDBLPCoref.fromFile(args(0))
    val venues = pubs.map(_.venue.string.trim).toSet.toSeq
    println(venues.contains("ISDA"))
    println(venues.mkString(","))

    val authorVenue = Map[String, Set[String]]() // venue -> set of authorid
    io.Source.fromFile(args(1)).getLines().foreach(l => {
      val fields = l.split("\t")
      if (venues.contains(fields(1))) {
	val authors = authorVenue.getOrElseUpdate(fields(1), Set())
	authors += fields(0)
      }
    })

    val coVenues = Map[Set[String], Int]() // two venues -> number of authors
    for (i <- 0 to (venues.size - 2)) {
      for (j <- (i + 1) to (venues.size - 1)) {
        val v1 = venues(i)
        val v2 = venues(j)
        val size = authorVenue(v1).intersect(authorVenue(v2)).size
        if (size > 0)
          coVenues.put(Set(v1, v2), size)
      }
    }

    coVenues.foreach(v => {
      println(v._1.mkString("\t") + "\t" + v._2)
    })

  }
}
