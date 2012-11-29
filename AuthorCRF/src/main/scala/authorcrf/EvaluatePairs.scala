package authorcrf
import cc.refectorie.user.sameer.util.coref._

object EvaluatePairs {

	def cluster(pairs : Seq[Pair]) {
		println("Cluster publication based on result of pairwise evaluation")
	}

	def evaluation(publications : Seq[Publication]) {
		println("Perform cluster evaluation here")
	}

	def build(pairs : Seq[Pair]): GenericEntityMap[Publication] = {
		val entMap = new GenericEntityMap[Publication]
		val pubs = collection.mutable.Set[Publication]()
		(pubs /: pairs)((s,p) => s ++= Set(p.publication1, p.publication2))
		pubs.foreach(p => entMap.addMention(p, entMap.numMentions.toLong))
		pairs.foreach(p => entMap.addCoreferentPair(p.publication1, p.publication2))
		entMap
	}

}
