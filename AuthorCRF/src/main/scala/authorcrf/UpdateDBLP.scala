package authorcrf

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import scala.xml._

object UpdateDBLP {
    classOf[org.postgresql.Driver]
    val db = DriverManager.getConnection("jdbc:postgresql://localhost/dblp","postgres","")
    val st = db.createStatement

	def updateFromKey(pubkey: String) : (String, String) = {
		val doc = XML.load("http://dblp.uni-trier.de/rec/bibtex/" + pubkey + ".xml")
		val pub = doc.child(1)
		val ty = pub.label
		val coauth = (pub \ "author").map( _.child.head).mkString(" ,")
		val title =  (pub \ "title").head.child.head.toString
		val venue = if( (pub \ "journal").length > 0) (pub \ "journal").head.child.head.toString
			else if((pub \ "booktitle").length > 0)  (pub \ "booktitle").head.child.head.toString
			else ""
		val year = (pub \ "year").head.child.head.toString.toInt
		val publ = new Publication(new Title(title), new CoAuthors(coauth), new Venue(venue), new Affiliation(""), "", year, ty, pubkey)
		updateFromPulication(publ)
		if(ty == "article") ("journal", "article") else if(ty=="incollection") ("booktitle","incollection") else ("booktitle", "inproceedings")
	}

	def insertAuthor(name : String) : Int = {
		val dbins = db.prepareStatement("INSERT into author(name) values(?)", Statement.RETURN_GENERATED_KEYS);
		dbins.setString(1,name)
		dbins.executeUpdate();
   		val rs = dbins.getGeneratedKeys();
    	rs.next();
   		rs.getInt(1);
	}

	def updateFromPulication(pub : Publication) {
		//val dbins = "INSERT into publication(pubkey, title, year) values('"+pub.pubkey+"','"+pub.title.string.replaceAll("'","\\\'")+"',"+pub.year+")"
		val dbins = db.prepareStatement("INSERT into publication(pubkey, title, year) values(?,?,?)", Statement.RETURN_GENERATED_KEYS);
		dbins.setString(1,pub.pubkey)
		dbins.setString(2,pub.title.string)
		dbins.setInt(3,pub.year)
		dbins.executeUpdate();
   		val rs = dbins.getGeneratedKeys();
    	rs.next();
   		val id = rs.getInt(1);
   		for(author <- pub.coAuthors.coauthors) {
   			val select = db.prepareStatement("SELECT * FROM author WHERE name = ?")
   			select.setString(1, author)
   			val rs = select.executeQuery()
   			val ai = if(rs.next) rs.getInt(1) else insertAuthor(author)
   			val authored = db.prepareStatement("INSERT into authored values(?,?)")
   			authored.setInt(1, ai)
   			authored.setInt(2, id)
   			authored.executeUpdate()
   		}
   		val insertString = pub.ty match {
   			case "article" => { "INSERT into article(pubid,journal) values(?,?)" }
   			case "incollection" => { "INSERT into inproceedings(pubid,booktitle) values(?,?)" }
   			case "inproceedings" => { "INSERT into incollection(pubid,booktitle) values(?,?)" } 
   		}
   		val insertVenue = db.prepareStatement(insertString)
   		insertVenue.setInt(1, id)
   		insertVenue.setString(2, pub.venue.string)
   		insertVenue.executeUpdate()
	}
}