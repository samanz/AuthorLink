package authorcrf
import cc.refectorie.user.sameer.util.coref._

object EvaluatePairs {

  var trueMap: GenericEntityMap[Publication] = null
  var predMap: GenericEntityMap[Publication] = null

  def cluster(pairs: Seq[Pair]) {
    if (predMap == null) predMap = build(pairs, false)
    predMap.entities.foreach(e => {
      println("=============")
      e._2.foreach(p => println(p.toString))
    })
  }

  def evaluation(pairs: Seq[Pair]) {
    if (predMap == null) predMap = build(pairs, false)
    if (trueMap == null) trueMap = build(pairs, true)
    val scorer = new Scorer[Publication]()
    scorer.addEntityMaps(predMap, trueMap)
    scorer.printInhouseScore
  }

  def clearMaps() {
    trueMap = null
    predMap = null
  }

  def build(pairs: Seq[Pair], useTrue: Boolean): GenericEntityMap[Publication] = {
    val entMap = new GenericEntityMap[Publication]
    val pubs = collection.mutable.Set[Publication]()
    (pubs /: pairs)((s, p) => s ++= Set(p.publication1, p.publication2))
    pubs.foreach(p => entMap.addMention(p, entMap.numMentions.toLong))
    if (useTrue) {
      pairs.foreach(p => if (p.attr[PubPairLabel].targetCategory.equals("YES")) entMap.addCoreferentPair(p.publication1, p.publication2))
    } else {
      pairs.foreach(p => if (p.attr[PubPairLabel].categoryValue.equals("YES")) entMap.addCoreferentPair(p.publication1, p.publication2))
    }
    entMap
  }

}

class Scorer[T] {

  val PW = new CorefEvaluator.Metric
  val B3 = new CorefEvaluator.B3Metric
  val MUC = new CorefEvaluator.Metric

  def addEntityMaps(predMap: GenericEntityMap[T], trueMap: GenericEntityMap[T]) {
    val pw = CorefEvaluator.Pairwise.evaluate(predMap, trueMap)
    val b3 = CorefEvaluator.BCubed.evaluate(predMap, trueMap)
    val muc = CorefEvaluator.MUC.evaluate(predMap, trueMap)
    B3.microAppend(b3)
    MUC.microAppend(muc)
    PW.microAppend(pw)
  }

  def printInhouseScore {
    print(B3.toString("B3") + "\n")
    print(MUC.toString("MUC") + "\n")
  }

}
