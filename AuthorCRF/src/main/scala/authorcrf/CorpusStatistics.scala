package authorcrf

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.Tokenizer
import cc.factorie.app.strings.Stopwords

object CorpusStatistics {
  def main(args: Array[String]) {
    val wordCount = collection.mutable.Map[String, Int]()
    scala.io.Source.fromFile(args(0)).getLines().foreach(l => {
      val d = new cc.factorie.app.nlp.Document("", l)
      Tokenizer.process(d)
      d.tokens.foreach(t => {
        val s = t.string.toLowerCase()
        if (!Stopwords.contains(s) && !s.matches("\\p{Punct}+")) {
          val c = wordCount.getOrElseUpdate(s, 0) + 1
          wordCount(s) = c
        }
      })
    })

    wordCount.toSeq.sortBy(_._1).foreach(wc => {
      println(wc._1 + " " + wc._2)
    })
  }
}
