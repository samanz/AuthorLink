package authorcrf

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import java.io._
import scala.xml._

object UpdateDBLP {
  classOf[org.postgresql.Driver]
  val db = DriverManager.getConnection("jdbc:postgresql://localhost/dblp","postgres","")
  val st = db.createStatement
  var note = ""
  
  def hasNote(node : Node) : Boolean = {
    val personRec = XML.load("http://dblp.uni-trier.de/rec/bibtex/" + node.child.head.toString + ".xml")
    if((personRec \ "www" \ "note").length > 0) {
       note = (personRec \ "www" \ "note").head.child.head.toString
       true
    } else false
  }

  def getNote : String = note

  def dumpAuthorsWithNote(name : String, file : String, start : Int = 0) {
    var ids = start
    val bw = new BufferedWriter(new FileWriter(new File(file)))
    val split = name.split(" ")
    val url = "/" + name(0).toLowerCase + "/" + split(0) + ":" + split(1)
    val per = XML.load("http://dblp.uni-trier.de/pers/xk" + url +".xml")
    val homonyms = (per \ "homonym")
    val pubs = (per \ "dblpkey")
    bw.write("#" + name + "\n")
    if(hasNote(pubs.head)) {
      val affil = getNote
      for(i <- 1 until pubs.length) {
        val pub = pubs(i)
        bw.write(pub.child.head.toString + "\t" + ids + "\t" + affil + "\n")
      }
    }
    for(homonym <- homonyms) {
      val hom = XML.load("http://dblp.uni-trier.de/pers/xk/" + homonym.head.child.head.toString +".xml")
      val pubs = (hom \ "dblpkey")
      if(hasNote(pubs.head)) {
        val affil = getNote
        for(i <- 1 until pubs.length) {
          val pub = pubs(i)
          bw.write(pub.child.head.toString + "\t" + ids + "\t" + affil + "\n")
        }
      }
      bw.flush()
      ids += 1
    }
  } 

  def main(args : Array[String]) : Unit = {
    val author = args.mkString(" ")
    dumpAuthorsWithNote(author, author.replaceAll(" ", "_")+".ann", 100)
  }

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