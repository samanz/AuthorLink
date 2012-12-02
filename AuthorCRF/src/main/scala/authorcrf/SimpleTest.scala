package authorcrf

import cc.factorie._
import collection.mutable.ArrayBuffer
import la.DenseTensor2
import optimize.{SGDTrainer, SampleRankExample}

class SimpleModel extends CombinedModel {
   val botAff = new DotTemplateWithStatistics2[FieldPairLabel,AffiliationFeatures] {
     lazy val weights = new la.DenseTensor2(PairLabelDomain.size, AffiliationFeaturesDomain.dimensionSize)
     def unroll1(fieldlabel: FieldPairLabel) = if(fieldlabel.field.attr.contains[AffiliationFeatures]) Factor(fieldlabel, fieldlabel.field.attr[AffiliationFeatures]) else Nil
     def unroll2(tf : AffiliationFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
  }
  val botTitl = new DotTemplateWithStatistics2[FieldPairLabel,TitleFeatures] {
    lazy val weights = new la.DenseTensor2(PairLabelDomain.size, TitleFeaturesDomain.dimensionSize)
    def unroll1(fieldlabel: FieldPairLabel) = if(fieldlabel.field.attr.contains[TitleFeatures]) Factor(fieldlabel, fieldlabel.field.attr[TitleFeatures]) else Nil
    def unroll2(tf : TitleFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
  }
  val middleAff = new DotTemplateWithStatistics2[PubPairLabel,FieldPairLabel] {
    lazy val weights = new DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)
    def unroll1(pairlabel : PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(0).attr[FieldPairLabel])
    def unroll2(fieldlabel : FieldPairLabel) = Nil //Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
  }
  val middleTitle = new TupleTemplateWithStatistics2[PubPairLabel,FieldPairLabel] {
    /*lazy val weights = new DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)*/
    def unroll1(pairlabel : PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(0).attr[FieldPairLabel])
    def unroll2(fieldlabel : FieldPairLabel) = Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
    def score(v1 : PubPairLabel#Value, v2 : FieldPairLabel#Value) : Double = {
     if (v2.category==v1.category) 1 else 0
    }
  }
  this += botAff
  //this += botTitl
  //this += middleAff
  this += middleTitle
}

class SimpleObjective extends HammingTemplate[PairLabel]

class SimpleTest {
  val model = new SimpleModel
  val objective = new SimpleObjective

  def similar(pub1 : Publication, pub2 : Publication) : Boolean = {
    var ma = false
    for(w <- pub1.block.split(" ")) {
      for (w2 <- pub2.block.split(" ")) {
        if (w==w2) ma = true
      }
    }
    ma
  }

  def fieldLabel(p : Pair) : String = if ( (p.publication1.attr[ClusterId].target == p.publication2.attr[ClusterId].target) ) "YES" else "NO"

  def initAffiliation(p : Pair) : FieldPair = {
    val fp = new FieldPair(p.publication1.affiliation, p.publication2.affiliation,p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    val feat = fp.attr += new AffiliationFeatures(fp)
    val f1 = p.publication1.affiliation
    val f2 = p.publication2.affiliation
    //if (p.publication1.attr[ClusterId].target==p.publication2.attr[ClusterId].target) feat += "YES" else feat += "NO"
    if(f1.string==f2.string) feat += "EXACT" else feat += "NOTEXACT"
    fp
  }

  def initTitle(p : Pair) : FieldPair = {
    val fp = new FieldPair(p.publication1.title, p.publication2.title,p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    fp.attr += new TitleFeatures(fp)
    fp
  }

  def initVenue(p : Pair) : FieldPair = {
    val fp = new FieldPair(p.publication1.venue, p.publication2.venue,p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    fp.attr += new VenueFeatures(fp)
    fp
  }

  def initCoauthor(p : Pair) : FieldPair = {
    val fp = new FieldPair(p.publication1.coAuthors, p.publication2.coAuthors,p)
    fp.attr += new FieldPairLabel(fp, fieldLabel(p))
    fp.attr += new CoAuthorsFeatures(fp)
    fp
  }

  def trainNew(trainFile : String) {
    var lf = LoadDBLPCoref.fromFile(trainFile)
    val pairs = new ArrayBuffer[Pair]()
    for(l1 <- 0 until lf.length) {
      val l = lf(l1)
      for(l2 <- (l1+1) until lf.length) {
        val k = lf(l2)
        if(similar(l,k)) {
          val p = new Pair(l,k)
          p.attr += new PubPairLabel(p, if(l.attr[ClusterId].target==k.attr[ClusterId].target) "YES" else "NO")
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

    val labels = trainPairs.map(_.fields(0).attr[FieldPairLabel]) ++ trainPairs.map(_.attr[PubPairLabel])
    trainPairs.map(_.attr[PubPairLabel]).foreach(_.setRandomly())
    //trainPairs.flatMap(_.fields.map(_.attr[FieldPairLabel])).foreach(_.setRandomly())
    labels.foreach( _.setRandomly() )// _.setCategory("O")(null))
    PairLabelDomain.foreach(println(_))
    val pieces = labels.map(l => new SampleRankExample(l, new GibbsSampler(model, objective)))
    val learner = new SGDTrainer(model, new optimize.AROW(model))
    val predictor = new IteratedConditionalModes[PairLabel](model)

    for (i <- 1 to 2) {
      println("--Iteration " + i)
      learner.processExamples(pieces)
      predictor.processAll(labels)
      println(model.botAff.weights)
      println(objective.accuracy(labels))
    }
    for (i <- 0 until 3; label <- labels) predictor.process(label)
    trainPairs.foreach{ x =>
      println(x.attr[PubPairLabel].targetCategory)
      println(x.attr[PubPairLabel].categoryValue)
      println(x.fields(0).attr[FieldPairLabel])
      println(x.fields(0).attr[AffiliationFeatures])
    }
    EvaluatePairs.clearMaps()
    EvaluatePairs.cluster(pairs)

    EvaluatePairs.evaluation(pairs)

  }
}

object SimpleTest extends SimpleTest {
  def main(args : Array[String]) : Unit = {
    trainNew(args(0))
  }
}