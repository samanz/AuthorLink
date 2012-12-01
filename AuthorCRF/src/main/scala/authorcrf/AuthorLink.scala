package authorcrf

import cc.factorie._
import cc.factorie.app.strings.Stopwords
import cc.factorie.optimize._
import scala.collection.mutable.ArrayBuffer

object PairLabelDomain extends CategoricalDomain[String]
abstract class PairLabel(initialValue:String) extends LabeledCategoricalVariable(initialValue) {
	  def domain = PairLabelDomain
}
class FieldPairLabel(val field:FieldPair, initialValue:String) extends PairLabel(initialValue)
class PubPairLabel(val pair:Pair, initialValue:String) extends PairLabel(initialValue)

class ClusterId(val id : Int, val target : Int)

// Field of the Document
class Field(val string : String) extends Attr
class Title(string : String) extends Field(string)
class CoAuthors(string : String) extends Field(string) {
	val coauthors = string.split(",")
}
class Venue(string : String) extends Field(string)
class Affiliation(string : String) extends Field(string)
class Publication(val title : Title, val coAuthors :  CoAuthors, val venue : Venue, val affiliation : Affiliation, val block : String, val year : Int = 1999, val ty : String = "", val pubkey : String = "") extends Attr {
	override def toString : String = {
		title.string + " : " + coAuthors.string + " " + venue.string + " "
	}
}

object Publication {
	def fromFields(fields : Seq[Field], block : String) : Publication = {
		var title : Title = null
		var venue : Venue = null
		var affil : Affiliation = null
		var coaut : CoAuthors = null

		for(field <- fields) {
			if(field.isInstanceOf[Title]) {
				title = field.asInstanceOf[Title]
			} else if (field.isInstanceOf[Venue]) {
				venue = field.asInstanceOf[Venue]
			} else if (field.isInstanceOf[Affiliation]) {
				affil = field.asInstanceOf[Affiliation]
			} else if (field.isInstanceOf[CoAuthors]) {
				coaut = field.asInstanceOf[CoAuthors]
			}
		}
		if(affil == null) affil = new Affiliation("")
		new Publication(title, coaut, venue, affil, block)
	}
}

class Pair(val publication1 : Publication, val publication2 : Publication) extends Attr {
	var fields : Seq[FieldPair] = null
	def setFields(f : Seq[FieldPair]) { fields = f }
}
object Pair {
	def apply(p1 : Publication, p2 :Publication) : Pair = {
		val f1 = Array[Field](p1.title,p1.coAuthors,p1.venue,p1.affiliation)
		val f2 = Array[Field](p2.title,p2.coAuthors,p2.venue,p2.affiliation)
		val pair = new Pair(p1, p2)
		val fp = (0 until f1.length).map(i => new FieldPair(f1(i),f2(i),pair))
		pair.setFields(fp)
		val same = (p1.attr[ClusterId].target==p2.attr[ClusterId].target)
		val ppl = new PubPairLabel(pair, if(same) "YES" else "NO")
		pair.attr += ppl
		pair
	} 
}
class FieldPair(val field1 : Field, val field2 : Field, val pair : Pair) extends Attr

object TitleFeaturesDomain extends CategoricalTensorDomain[String]
object CoAuthorsFeaturesDomain extends CategoricalTensorDomain[String]
object AffiliationFeaturesDomain extends CategoricalTensorDomain[String]
object VenueFeaturesDomain extends CategoricalTensorDomain[String]

abstract class AuthorFeatures(val field : FieldPair) extends BinaryFeatureVectorVariable[String] {
  def domain : CategoricalTensorDomain[String] 
  override def skipNonCategories = true
}
class TitleFeatures(field : FieldPair) extends AuthorFeatures(field) {
	def domain = TitleFeaturesDomain
}
class CoAuthorsFeatures(field : FieldPair) extends AuthorFeatures(field) {
	def domain = CoAuthorsFeaturesDomain
}
class AffiliationFeatures(field : FieldPair) extends AuthorFeatures(field) {
	def domain = AffiliationFeaturesDomain
}
class VenueFeatures(field : FieldPair) extends AuthorFeatures(field) {
	def domain = VenueFeaturesDomain
}

class AuthorLinkModel extends CombinedModel {
	val bias = new DotTemplateWithStatistics1[PairLabel] {
		factorName = "Bias"
		lazy val weights = new la.DenseTensor1(PairLabelDomain.size)
	}
	// Factor between publication pair variable and field variables
	val fieldTemplate1 = new DotTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
		factorName = "Field1"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)
	    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(0).attr[FieldPairLabel])
	    def unroll2(fieldlabel: FieldPairLabel) = Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
	}
	// Factor between publication pair variable and field variables
	val fieldTemplate2 = new DotTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
		factorName = "Field2"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)
	    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(1).attr[FieldPairLabel])
	    def unroll2(fieldlabel: FieldPairLabel) = Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
	}
	// Factor between publication pair variable and field variables
	val fieldTemplate3 = new DotTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
		factorName = "Field3"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)
	    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(2).attr[FieldPairLabel])
	    def unroll2(fieldlabel: FieldPairLabel) = Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
	}
	// Factor between publication pair variable and field variables
	val fieldTemplate4 = new DotTemplateWithStatistics2[PubPairLabel, FieldPairLabel] {
		factorName = "Field4"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, PairLabelDomain.size)
	    def unroll1(pairlabel: PubPairLabel) = Factor(pairlabel, pairlabel.pair.fields(3).attr[FieldPairLabel])
	    def unroll2(fieldlabel: FieldPairLabel) = Factor(fieldlabel.field.pair.attr[PubPairLabel], fieldlabel)
	}
	// Factor between field and TitleFeatures
	val TitleTemplate = new DotTemplateWithStatistics2[FieldPairLabel, TitleFeatures] {
		factorName = "Title"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, TitleFeaturesDomain.dimensionSize)
		def unroll1(fieldlabel : FieldPairLabel) = if(fieldlabel.field.attr.contains[TitleFeatures]) Factor(fieldlabel, fieldlabel.field.attr[TitleFeatures]) else Nil
		def unroll2(tf : TitleFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
	}
	// Factor between field and VenueFeatures
	val VenueTemplate = new DotTemplateWithStatistics2[FieldPairLabel, VenueFeatures] {
		factorName = "Venue"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, VenueFeaturesDomain.dimensionSize)
		def unroll1(fieldlabel : FieldPairLabel) = if(fieldlabel.field.attr.contains[VenueFeatures]) Factor(fieldlabel, fieldlabel.field.attr[VenueFeatures]) else Nil
		def unroll2(tf : VenueFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
	}
	// Factor between field and CoAuthorFeatures
	val CoAuthorTemplate = new DotTemplateWithStatistics2[FieldPairLabel, CoAuthorsFeatures] {
		factorName = "CoAuthor"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, CoAuthorsFeaturesDomain.dimensionSize)
		def unroll1(fieldlabel : FieldPairLabel) = if(fieldlabel.field.attr.contains[CoAuthorsFeatures]) Factor(fieldlabel, fieldlabel.field.attr[CoAuthorsFeatures]) else Nil
		def unroll2(tf : CoAuthorsFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
	}
	// Factor between field and AffiliationFeatures
	val AffiliationTemplate = new DotTemplateWithStatistics2[FieldPairLabel, AffiliationFeatures] {
		factorName = "Affiliation"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, AffiliationFeaturesDomain.dimensionSize)
		def unroll1(fieldlabel : FieldPairLabel) = { 
			println("Got here")
			if(fieldlabel.field.attr.contains[AffiliationFeatures]) { println("Create Factor"); Factor(fieldlabel, fieldlabel.field.attr[AffiliationFeatures]) } else Nil
		}
		def unroll2(tf : AffiliationFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
	}
}

class AuthorLinkObjective extends HammingTemplate[PairLabel]

class AuthorLink {
	val model = new AuthorLinkModel
	val objective = new AuthorLinkObjective
	var titleStat:Map[String, Int] = null
	var venueStat:Map[String, Int] = null
	val statThreshold = 10

	def similar(p1 : Publication, p2 : Publication) : Boolean = { true }

	def initTitleFeatures(fp : FieldPair) {
		val f = new TitleFeatures(fp)
		val v1 = fp.field1.asInstanceOf[Title].string
		val v2 = fp.field2.asInstanceOf[Title].string
		val v1WithoutStop = v1.toLowerCase().replaceAll("\\p{Punct}+", " ").split(" +").filterNot(Stopwords.contains(_))
		val v2WithoutStop = v2.toLowerCase().replaceAll("\\p{Punct}+", " ").split(" +").filterNot(Stopwords.contains(_))
		if (v1.equalsIgnoreCase(v2)) f += "Title_Same"
		f += "Title_Overlap_" + v1WithoutStop.toSet.intersect(v2WithoutStop.toSet) // TODO: bin?
		for (w1 <- v1WithoutStop.toSet[String].filter(w => titleStat.contains(w) && titleStat(w) > statThreshold)) {
		  for (w2 <- v2WithoutStop.toSet[String]filter(w => titleStat.contains(w) && titleStat(w) > statThreshold)) {
		    f += "Title_WordPair_" + (if (w1 < w2) (w1 + "_" + w2) else (w2 + "_" + w1))
		  }
		}
		// TODO: use lucene score?
		fp.attr += f
	}

	def initCoAuthorsFeatures(fp : FieldPair) {
		//fp.attr += new CoAuthorsFeatures(fp)
	}

	def initAffiliationFeatures(fp : FieldPair) {
		fp.attr += new AffiliationFeatures(fp)
		if(fp.field1.asInstanceOf[Affiliation].string==fp.field2.asInstanceOf[Affiliation].string) fp.attr[AffiliationFeatures] += "EXACT"
	}

	def initVenueFeatures(fp : FieldPair) {
		val f = new VenueFeatures(fp)
		val v1 = fp.field1.asInstanceOf[Venue].string
		val v2 = fp.field2.asInstanceOf[Venue].string
		val v1WithoutStop = v1.toLowerCase().replaceAll("\\p{Punct}+", " ").split(" +").filterNot(Stopwords.contains(_))
		val v2WithoutStop = v2.toLowerCase().replaceAll("\\p{Punct}+", " ").split(" +").filterNot(Stopwords.contains(_))
		if (v1.equalsIgnoreCase(v2)) f += "Venue_Same"
		f += "Venue_Overlap_" + v1WithoutStop.toSet.intersect(v2WithoutStop.toSet)
		for (w1 <- v1WithoutStop.toSet[String].filter(w => venueStat.contains(w) && venueStat(w) > statThreshold)) {
		  for (w2 <- v2WithoutStop.toSet[String].filter(w => venueStat.contains(w) && venueStat(w) > statThreshold)) {
		    f += "Venue_WordPair_" + (if (w1 < w2) (w1 + "_" + w2) else (w2 + "_" + w1))
		  }
		}
		fp.attr += f
	}

	def pairAndInitFeatures(pubs : Seq[Publication]) : Seq[Pair] = {
		var pairs = ArrayBuffer[Pair]()
		for(p1 <- 0 until pubs.length) {
			val p1l = pubs(p1)
			for(p2 <- p1+1 until pubs.length) {
				val p2l = pubs(p2)
				if(similar(p1l,p2l)) pairs += Pair(p1l, p2l)
			}
		}
		for(pair <- pairs) {
			for(fp <- pair.fields) {
				fp.attr += new FieldPairLabel(fp, pair.attr[PairLabel].targetCategory)
				fp.field1 match {
					case title : Title => initTitleFeatures(fp)
					case venue : Venue => initVenueFeatures(fp)
					case affil : Affiliation => initAffiliationFeatures(fp)
					case coauth : CoAuthors => initCoAuthorsFeatures(fp)
				}
			}
		}
		pairs.toSeq
	} 

	def test(testFile : String) {
		val testPublications = LoadDBLPCoref.fromFile(testFile)
		val testingPairs = pairAndInitFeatures(testPublications) 
      	
      	val testFieldLabels = testingPairs.map(_.fields).flatten.map(_.attr[FieldPairLabel])
      	val testPairLabels = testingPairs.map(_.attr[PubPairLabel])

		(testFieldLabels ++ testPairLabels).foreach(_.setRandomly())

      	val predictor = new IteratedConditionalModes[PairLabel](model) // {temperature=0.01}
      
      	for (i <- 0 until 3; label <- testFieldLabels) predictor.process(label)
      	for (i <- 0 until 3; label <- testPairLabels) predictor.process(label)


      	EvaluatePairs.clearMaps()
      	EvaluatePairs.cluster(testingPairs)

      	EvaluatePairs.evaluation(testingPairs)
	}

	def train(trainFile : String, testFile : String ) {
		val trainPublications = LoadDBLPCoref.fromFile(trainFile)
		val testPublications = LoadDBLPCoref.fromFile(testFile)

		println("Train Publications Size " + trainPublications.length)

		val trainingPairs = pairAndInitFeatures(trainPublications)
		val testingPairs = pairAndInitFeatures(testPublications) 
      	
      	val trainFieldLabels = trainingPairs.map(_.fields).flatten.map(_.attr[FieldPairLabel])
      	val trainPairLabels = trainingPairs.map(_.attr[PubPairLabel])
      	val testFieldLabels = testingPairs.map(_.fields).flatten.map(_.attr[FieldPairLabel])
      	val testPairLabels = trainingPairs.map(_.attr[PubPairLabel])

		(trainFieldLabels ++ trainPairLabels ++ testFieldLabels ++ testPairLabels).foreach(_.setRandomly())
      	
      	val learner = new SampleRankTrainer(new GibbsSampler(model, objective), new ConfidenceWeighting(model, 0.01))
      	val predictor = new IteratedConditionalModes[PairLabel](model) // {temperature=0.01}
      
      	for (i <- 1 until 15) {
      		println("--Iteration " + i)
        	learner.processContexts(trainFieldLabels)
        	learner.processContexts(trainPairLabels)
        	predictor.processAll(testFieldLabels)
        	predictor.processAll(testPairLabels)
        	println(model.AffiliationTemplate.weights)
      	}
      	for (i <- 0 until 3; label <- testFieldLabels) predictor.process(label)
      	for (i <- 0 until 3; label <- testPairLabels) predictor.process(label)

      	EvaluatePairs.clearMaps()
      	EvaluatePairs.cluster(trainingPairs)
      	EvaluatePairs.cluster(testingPairs)

      	EvaluatePairs.evaluation(trainingPairs)
      	EvaluatePairs.evaluation(testingPairs)

	}

}

object AuthorLink extends AuthorLink {
	def main(args : Array[String]) : Unit = {
  	import cc.factorie.util.DefaultCmdOptions
    	object opts extends DefaultCmdOptions {
      		val trainFile =     	new CmdOption("train", "eng.train", "FILE", "CoNLL formatted training file.")
      		val testFile  =     	new CmdOption("test",  "eng.testb", "FILE", "CoNLL formatted test file.")
      		val sigmaSq  =     	new CmdOption("sigmaSq",  "10", "REAL", "Value for regularization constant for BP.")
      		val modelDir =      	new CmdOption("model", "chainner.factorie", "DIR", "Directory for saving or loading model.")
      		val justTest = 		new CmdOption("justTest", "No Training, only Testing.")
      		val titleStat = 	new CmdOption("titleStat", "titles.txt", "FILE", "counts of words in publication titles.")
      		val venueStat = 	new CmdOption("venueStat", "venues.txt", "FILE", "counts of words in venues.")
   		}
    	opts.parse(args)

	titleStat = io.Source.fromFile(opts.titleStat.value).getLines().map(l => {
	  val f = l.split(" ")
	  f(0) -> f(1).toInt
	}).toMap

	venueStat = io.Source.fromFile(opts.venueStat.value).getLines().map(l => {
	  val f = l.split(" ")
	  f(0) -> f(1).toInt
	}).toMap

    	if(opts.justTest.wasInvoked) {
    		model.load(opts.modelDir.value)
    		test(opts.testFile.value)
    	} else {
    		train(opts.trainFile.value, opts.testFile.value)
    		//model.save(opts.modelDir.value)
    	}
	}
}
