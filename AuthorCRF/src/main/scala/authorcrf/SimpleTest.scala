package authorcrf

import cc.factorie._
import cc.factorie.app.strings.Stopwords
import collection.mutable.ArrayBuffer
import la.DenseTensor2
import optimize.{ SGDTrainer, SampleRankExample }
import cc.factorie.app.strings._

class SimpleModel extends CombinedModel {
  val botAff = new DotTemplateWithStatistics2[FieldPairLabel, AffiliationFeatures] {
    lazy val weights = new la.DenseTensor2(PairLabelDomain.size, AffiliationFeaturesDomain.dimensionSize)
    def unroll1(fieldlabel: FieldPairLabel) = if (fieldlabel.field.attr.contains[AffiliationFeatures]) Factor(fieldlabel, fieldlabel.field.attr[AffiliationFeatures]) else Nil
    def unroll2(tf: AffiliationFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
  }
  val botTitl = new DotTemplateWithStatistics3[FieldPairLabel, TitleFeatures, TitleFeatures] {
    lazy val weights = new la.DenseTensor3(PairLabelDomain.size, TitleFeaturesDomain.dimensionSize, TitleFeaturesDomain.dimensionSize)
    def unroll1(fieldlabel: FieldPairLabel) = if (fieldlabel.field.field2.attr.contains[TitleFeatures]) Factor(fieldlabel, fieldlabel.field.field1.attr[TitleFeatures], fieldlabel.field.field2.attr[TitleFeatures]) else Nil
    def unroll2(tf: TitleFeatures) = Nil //Factor(tf.field.attr[FieldPairLabel], tf, tf)
    def unroll3(tf: TitleFeatures) = Nil //Factor(tf.field.attr[FieldPairLabel], tf, tf)
  }
  val botVen = new DotTemplateWithStatistics2[FieldPairLabel, VenueFeatures] {
    lazy val weights = new la.DenseTensor2(PairLabelDomain.size, VenueFeaturesDomain.dimensionSize)
    def unroll1(fieldlabel: FieldPairLabel) = if (fieldlabel.field.attr.contains[VenueFeatures]) Factor(fieldlabel, fieldlabel.field.attr[VenueFeatures]) else Nil
    def unroll2(tf: VenueFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
  }

  val middleTitle = new TupleTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
    /*lazy val weights = new DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)*/
    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(1).attr[FieldPairLabel])
    def unroll2(fieldlabel: FieldPairLabel) = Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
    def score(v1: PubPairLabel#Value, v2: FieldPairLabel#Value): Double = {
      if(v1.category == v2.category) 1.0 else 0.0
    }
  }
  val middleAff = new TupleTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
    /*lazy val weights = new DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)*/
    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(0).attr[FieldPairLabel])
    def unroll2(fieldlabel: FieldPairLabel) = if(fieldlabel.field.field1.isInstanceOf[Affiliation]) Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel) else Nil
    def score(v1: PubPairLabel#Value, v2: FieldPairLabel#Value): Double = {
      if(v1.category==v2.category) 1.0 else 0.0
    }
  }
  val middleVen = new TupleTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
    /*lazy val weights = new DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)*/
    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(2).attr[FieldPairLabel])
    def unroll2(fieldlabel: FieldPairLabel) = if(fieldlabel.field.field1.isInstanceOf[Venue]) Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel) else Nil
    def score(v1: PubPairLabel#Value, v2: FieldPairLabel#Value): Double = {
      if(v1.category==v2.category) 1.0 else 0.0
    }
  }

  //this += botAff
  //this += botTitl
  //this += middleAff
  //this += middleTitle
  this += middleVen
  this += botVen
}

class SimpleObjective extends HammingTemplate[PairLabel]

class SimpleTest {
  val model = new SimpleModel
  val objective = new SimpleObjective

  var titleStat: Map[String, Int] = null
  var venueStat: Map[String, Int] = null
  val statThreshold = 10
  var venuePairStat: Map[Set[String], Int] = null

  def similar(pub1: Publication, pub2: Publication): Boolean = {
    val w = pub1.block.split(" ")
    val w2 = pub2.block.split(" ")
    (w.head==w2.head && w.last==w2.last)
  }

  def fieldLabel(p: Pair): String = if ((p.publication1.attr[ClusterId].target == p.publication2.attr[ClusterId].target)) "YES" else "NO"

  def initAffiliation(p: Pair): FieldPair = {
    val fp = new FieldPair(p.publication1.affiliation, p.publication2.affiliation, p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    val feat = fp.attr += new AffiliationFeatures(fp)
    val f1 = p.publication1.affiliation
    val f2 = p.publication2.affiliation
    //if (p.publication1.attr[ClusterId].target==p.publication2.attr[ClusterId].target) feat += "YES" else feat += "NO"
    if (f1.string == f2.string) feat += "EXACT" else feat += "NOTEXACT"
    fp
  }

  def initTitle(p: Pair): FieldPair = {
    val fp = new FieldPair(p.publication1.title, p.publication2.title, p)    
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    fp
  }  
  def initTitlePub(p : Publication) {
    val f = p.title.attr += new TitleFeatures(p.title)
    val v1 = p.title.string
    val v1WithoutStop = v1.toLowerCase().replaceAll("\\p{Punct}+", " ").split(" +").filter( x => !Stopwords.contains(x) && titleStat.contains(x) && titleStat(x) > 10000)
    /*if (v1.equalsIgnoreCase(v2)) f += "Title_Same"
    f += "Title_Overlap_" + v1WithoutStop.toSet.intersect(v2WithoutStop.toSet) // TODO: bin?
    for (w1 <- v1WithoutStop.toSet[String].filter(w => titleStat.contains(w) && titleStat(w) > statThreshold)) {
      for (w2 <- v2WithoutStop.toSet[String] filter (w => titleStat.contains(w) && titleStat(w) > statThreshold)) {
        f += "Title_WordPair_" + (if (w1 < w2) (w1 + "_" + w2) else (w2 + "_" + w1))
      }
    }
    if (p.publication1.attr[ClusterId].target==p.publication2.attr[ClusterId].target) f += "YES" else f += "NO"
    */
    v1WithoutStop.foreach( f += Stemmer(_) )
  }

  def initVenue(p: Pair): FieldPair = {
    val fp = new FieldPair(p.publication1.venue, p.publication2.venue, p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    val f = new VenueFeatures(fp)
    fp.attr += f
    val v1 = p.publication1.venue.string
    val v2 = p.publication2.venue.string
    if (v1.equalsIgnoreCase(v2))
     f += "Same_Venu"
    if (venuePairStat.contains(Set(v1, v2))) {
      val v = venuePairStat(Set(v1, v2))
      if (v == 1)
        f += "Venue_1"
      else if (v > 1 && v <= 4)
        f += "Venue_2to4"
      else if (v > 4 && v <= 16)
        f += "Venue_5to16"
      else if (v > 16 && v <= 64)
	f += "Venue_17to64"
      else if (v > 64 && v <= 256)
	f += "Venue_65to256"
      else
        f += "Venue_257"
    } else {
      f += "Venue_0"
    }
    fp
  }

  def initCoauthor(p: Pair): FieldPair = {
    val fp = new FieldPair(p.publication1.coAuthors, p.publication2.coAuthors, p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    fp.attr += new CoAuthorsFeatures(fp)
    //CoAuthorStats.addCoAuthorFeatures(p)
    fp
  }

  def trainNew(trainFile: String) {
    var lf = LoadDBLPCoref.fromFile(trainFile)
    //lf.foreach(initTitlePub(_))
    val pairs = new ArrayBuffer[Pair]()
    for (l1 <- 0 until lf.length) {
      val l = lf(l1)
      for (l2 <- (l1 + 1) until lf.length) {
        val k = lf(l2)
        if (similar(l, k)) {
          val p = new Pair(l, k)
          p.attr += new PubPairLabel(p, if (l.attr[ClusterId].target == k.attr[ClusterId].target) "YES" else "NO")
          val aff = initAffiliation(p)
          val tit = initTitle(p)
          val ven = initVenue(p)
          val coa = initCoauthor(p)
          /*val fp = new FieldPair(new Affiliation(""), new Affiliation(""), p)
          val fp1 = new FieldPair(new Title(""), new Title(""), p)
          fp.attr += new FieldPairLabel(fp, if(l.attr[ClusterId].target==k.attr[ClusterId].target) "YES" else "NO")
          fp1.attr += new FieldPairLabel(fp, if(l.attr[ClusterId].target==k.attr[ClusterId].target) "YES" else "NO")
          val feat = fp.attr += new AffiliationFeatures(fp)
          val feat1 = fp1.attr += new TitleFeatures(fp)
          if ( (l.attr[ClusterId].target == k.attr[ClusterId].target) ) feat += "YES" else feat += "NO"
          */
          p.fields = Array(aff, tit, ven, coa)
          pairs += p
        }
      }
    }
    val trainPairs = pairs.toSeq

    val labels = /*trainPairs.map(_.fields(0).attr[FieldPairLabel]) ++*/ trainPairs.map(_.fields(2).attr[FieldPairLabel]) ++ trainPairs.map(_.attr[PubPairLabel])
    trainPairs.map(_.attr[PubPairLabel]).foreach(_.setRandomly())
    //trainPairs.flatMap(_.fields.map(_.attr[FieldPairLabel])).foreach(_.setRandomly())
    labels.foreach(_.setRandomly()) // _.setCategory("O")(null))
    PairLabelDomain.foreach(println(_))
    val pieces = labels.map(l => new SampleRankExample(l, new GibbsSampler(model, objective)))
    val learner = new SGDTrainer(model, new optimize.AROW(model))
    val predictor = new IteratedConditionalModes[PairLabel](model)
      println(model.botTitl.weights.size)

    for (i <- 1 to 4) {
      println("--Iteration " + i)
      learner.processExamples(pieces)
      predictor.processAll(labels)
      println(model.botAff.weights)
      println(model.botVen.weights)
      println(objective.accuracy(labels.filter(_.isInstanceOf[PubPairLabel])))
      println(objective.accuracy(labels))
    }
    /*for (i <- 0 until 3; label <- labels) predictor.process(label)
    trainPairs.foreach { x =>
      println(x.fields(1).attr[FieldPairLabel])
      println(x.attr[PubPairLabel].targetCategory)
      println("=============")
    }*/
    /*trainPairs.foreach { x =>
      println(x.attr[PubPairLabel].targetCategory)
      println(x.attr[PubPairLabel].categoryValue)
      println(x.fields(0).attr[FieldPairLabel])
      println(x.fields(0).attr[AffiliationFeatures])
    }*/
    EvaluatePairs.clearMaps()
    EvaluatePairs.cluster(pairs)

    EvaluatePairs.evaluation(pairs)

  }

def allFalse(trainFile : String) {
    var lf = LoadDBLPCoref.fromFile(trainFile)
    lf.foreach(initTitlePub(_))
    val pairs = new ArrayBuffer[Pair]()
    for (l1 <- 0 until lf.length) {
      val l = lf(l1)
      for (l2 <- (l1 + 1) until lf.length) {
        val k = lf(l2)
        if (similar(l, k)) {
          val p = new Pair(l, k)
          p.attr += new PubPairLabel(p, if (l.attr[ClusterId].target == k.attr[ClusterId].target) "YES" else "NO")
          //p.fields = Array(aff, tit, ven, coa)
          pairs += p
        }
      }
    }
    val trainPairs = pairs.toSeq
    trainPairs.map(_.attr[PubPairLabel]).foreach(_.setRandomly())

    EvaluatePairs.clearMaps()
    EvaluatePairs.cluster(pairs)

    EvaluatePairs.evaluation(pairs)
  }

def printTruth(trainFile : String) {
    var lf = LoadDBLPCoref.fromFile(trainFile)
    lf.foreach(initTitlePub(_))
    val pairs = new ArrayBuffer[Pair]()
    for (l1 <- 0 until lf.length) {
      val l = lf(l1)
      for (l2 <- (l1 + 1) until lf.length) {
        val k = lf(l2)
        if (similar(l, k)) {
          val p = new Pair(l, k)
          p.attr += new PubPairLabel(p, if (l.attr[ClusterId].target == k.attr[ClusterId].target) "YES" else "NO")
          //p.fields = Array(aff, tit, ven, coa)
          pairs += p
        }
      }
    }
    val trainPairs = pairs.toSeq

    EvaluatePairs.clearMaps()
    EvaluatePairs.cluster(pairs)

    EvaluatePairs.evaluation(pairs)
  }
}

object SimpleTest extends SimpleTest {
  def main(args: Array[String]): Unit = {
    titleStat = io.Source.fromFile("titles.count").getLines().map(l => {
      val f = l.split(" ")
      f(0) -> f(1).toInt
    }).toMap

    venueStat = io.Source.fromFile("venues.count").getLines().map(l => {
      val f = l.split(" ")
      f(0) -> f(1).toInt
    }).toMap

    venuePairStat = io.Source.fromFile("venuePair.count").getLines().map(l => {
      val f = l.split("\t")
      Set(f(0), f(1)) -> f(2).toInt
    }).toMap

    trainNew(args(0))
    //printTruth(args(0))
  }
}
