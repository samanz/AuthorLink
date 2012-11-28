package authorcrf

import java.sql.{Connection, DriverManager, ResultSet}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object LoadDBLPCoref {
    classOf[org.postgresql.Driver]
    val db = DriverManager.getConnection("jdbc:postgresql://localhost/dblp","postgres","")
    val st = db.createStatement

    def getFields(pubkey : String) : Seq[Field] = {
    	val res = st.executeQuery(
    		   "SELECT name, title, booktitle, year from author, publication, authored, inproceedings " +
    		   "WHERE pubkey='"+pubkey+"' AND inproceedings.pubid=publication.pubid " +
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
    	Array[Field](new Title(title), new CoAuthors(authors), new Venue(venue))
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
					publications += Publication.fromFields(fields, block)
				}
			}
		}
		publications.toSeq
	}
}