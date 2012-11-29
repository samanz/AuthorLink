package authorcrf

import java.sql.{Connection, DriverManager, ResultSet}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object LoadDBLPCoref {
    classOf[org.postgresql.Driver]
    val db = DriverManager.getConnection("jdbc:postgresql://localhost/dblp","postgres","")
    val st = db.createStatement

    def getFields(pubkey : String) : Seq[Field] = {
    	var field = ""
    	var table = ""
    	getType(pubkey) match {
    		case "proceeding" => { field = "booktitle"; table = "inproceedings" }
    		case "collection" => { field = "booktitle"; table = "incollection" }
    		case "article" => { field = "journal"; table = "article" }
    		case _ => { println("Match Error " + pubkey); val ft = UpdateDBLP.updateFromKey(pubkey); field = ft._1; table = ft._2 }
    	}
    	if(field != "") {
    		val res = st.executeQuery(
    		   "SELECT name, title, " + field + ", year from author, publication, authored, " + table + " " +
    		   "WHERE pubkey='"+pubkey+"' AND " + table +".pubid=publication.pubid " +
    		   "AND authored.pubid=publication.pubid AND author.id=authored.id")
    		var authors = ""
    		var title = ""
    		var venue = ""
    		while (res.next) {
    			authors += res.getString(1).toString +","
    			title = res.getString(2)
    			venue = res.getString(3)
    		}
    		if(title == "") println("Not found: " + pubkey)
    		Array[Field](new Title(title), new CoAuthors(authors.init), new Venue(venue))
   		} else Array[Field]()
   	}

   	def getType(pubkey : String) : String = {
   		val proc = st.executeQuery(
    		"SELECT booktitle from publication, inproceedings " +
    		   "WHERE pubkey='"+pubkey+"' AND inproceedings.pubid=publication.pubid")
    	var ty = ""
    	while (proc.next) { ty = "proceeding" }
   	   	val coll = st.executeQuery(
    		"SELECT booktitle from publication, incollection " +
    		   "WHERE pubkey='"+pubkey+"' AND incollection.pubid=publication.pubid")

      	while (coll.next) {	ty = "collection" }
   	   	val article = st.executeQuery(
    		"SELECT journal from publication, article " +
    		   "WHERE pubkey='"+pubkey+"' AND article.pubid=publication.pubid")
       	while (article.next) { ty = "article"	}
    	ty
   	}

	def fromFile(file : String) : Seq[Publication] = {
		var block = ""
		val publications = new ArrayBuffer[Publication]()
		for(line <- Source.fromFile(file).getLines()) {
			if(line.startsWith("#")) block = line.substring(1)
			else if(line.trim.length > 0) {
				val split = line.split("\t")
				if(split.length > 1) {
					val fields = if(split.length > 2) (getFields(split(0)) ++ Array[Field](new Affiliation(split(2)))) else getFields(split(0))
					if(fields.length > 2) {
						val pub = Publication.fromFields(fields, block)
						pub.attr += new ClusterId(split(1).toInt,split(1).toInt)
						publications += pub
					}
				}
			}
		}
		publications.toSeq
	}
}
