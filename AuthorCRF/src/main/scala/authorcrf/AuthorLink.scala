package authorcrf

import cc.factorie._
import cc.factorie.optimize._

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

class Pair(val fields : Seq[FieldPair], val publication1 : Publication, val publication2 : Publication) extends Attr
class FieldPair(val field1 : Field, val field2 : Field, val pair : Pair) extends Attr

object TitleFeaturesDomain extends CategoricalTensorDomain[String]
object CoAuthorsFeaturesDomain extends CategoricalTensorDomain[String]
object AffiliationFeaturesDomain extends CategoricalTensorDomain[String]
object VenueFeaturesDomain extends CategoricalTensorDomain[String]

abstract class AuthorFeatures(val field : Field) extends BinaryFeatureVectorVariable[String] {
  def domain : CategoricalTensorDomain[String] 
  override def skipNonCategories = true
}
class TitleFeatures(field : Field) extends AuthorFeatures(field) {
	def domain = TitleFeaturesDomain
}
class CoAuthorsFeatures(field : Field) extends AuthorFeatures(field) {
	def domain = CoAuthorsFeaturesDomain
}
class AffiliationFeatures(field : Field) extends AuthorFeatures(field) {
	def domain = AffiliationFeaturesDomain
}
class VenueFeatures(field : Field) extends AuthorFeatures(field) {
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
		factorName = "Title"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, CoAuthorsFeaturesDomain.dimensionSize)
		def unroll1(fieldlabel : FieldPairLabel) = if(fieldlabel.field.attr.contains[CoAuthorsFeatures]) Factor(fieldlabel, fieldlabel.field.attr[CoAuthorsFeatures]) else Nil
		def unroll2(tf : CoAuthorsFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
	}
	// Factor between field and AffiliationFeatures
	val AffiliationTemplate = new DotTemplateWithStatistics2[FieldPairLabel, AffiliationFeatures] {
		factorName = "Title"
		lazy val weights = new la.DenseTensor2(PairLabelDomain.size, AffiliationFeaturesDomain.dimensionSize)
		def unroll1(fieldlabel : FieldPairLabel) = if(fieldlabel.field.attr.contains[AffiliationFeatures]) Factor(fieldlabel, fieldlabel.field.attr[AffiliationFeatures]) else Nil
		def unroll2(tf : AffiliationFeatures) = Factor(tf.field.attr[FieldPairLabel], tf)
	}
}

class AuthorLinkObjective extends HammingTemplate[PubPairLabel]

class AuthorLink {
	val model = new AuthorLinkModel
	val objective = new AuthorLinkObjective

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

	def pairAndInitFeatures(pairs : Seq[Publication]) : Seq[Pair] = {
		Array[Pair]()
	} 


	def train(trainFile : String, testFile : String ) {
		val trainPublications = LoadDBLPCoref.fromFile(trainFile)
		val testPublications = LoadDBLPCoref.fromFile(testFile)

		trainPublications.foreach{ println(_) }

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
      
      	for (iteration <- 1 until 15) {
        	learner.processContexts(trainFieldLabels)
        	learner.processContexts(trainPairLabels)
        	predictor.processAll(testFieldLabels)
        	predictor.processAll(testPairLabels)
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
   		}
    	opts.parse(args)

    	if(opts.justTest.wasInvoked) {
    		model.load(opts.modelDir.value)
    		test(opts.testFile.value)
    	} else {
    		train(opts.trainFile.value, opts.testFile.value)
    		//model.save(opts.modelDir.value)
    	}
	}
}
