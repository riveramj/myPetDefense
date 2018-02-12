package com.mypetdefense.util 

import net.liftweb._
  import common._
  import util._
import com.mypetdefense.actor._

object SpecialEmailSender extends Loggable {
  def send5kEmails = {
    val nameEmailDog = List(
      ("Chelsea","chenson724@comcast.net","DALTON-CONNOR"),
      ("Cindy","cbair59@comcast.net","Bo"),
      ("Brooke","Brookerobinson95@gmail.com","Bacon"),
      ("Todd","Taprest@comcast.net","Sookie"),
      ("Ashley","Ashleyprescott90@gmail.com","Zoey"),
      ("Leah","doodlebug_20@hotmail.com","Jerry"),
      ("Leah","doodlebug_20@hotmail.com","Penelope"),
      ("Janise","Janise_jordan@yahoo.com","Hope"),
      ("Wade","wade.cotton01@gmail.com","Cooper"),
      ("Cecelia","Ceceliaamorgan@gmail.com","Marlowe"),
      ("Julie","Karnoscakj@yahoo.com","Sadie"),
      ("Rhonda","Rhonda.cotton01@gmail.com","Jack"),
      ("Mara","Jjust1@bellsouth.net","LUCYLU-SCARLETT"),
      ("David","Davidandkate.phillips@gmail.com","Leber"),
      ("Katie","Katie.h.tracy@gmail.com","ROB-KATE"),
      ("Megan","Megan.l.manly@mac.com","zeke"),
      ("Stephanie","Scarani@columbiacountyso.org","Loki"),
      ("Megan","meganwinzeler@gmail.com","Cairo"),
      ("Amanda","Amandalhall@yahoo.com","Sam"),
      ("Kara","Stymie09@comcast.net","RUBIE-DIXIE-PIPER"),
      ("Allyn","Amccann4131@gmail.com","Sage"),
      ("Jesi","jesi.cotton5@yahoo.com","Baxter"),
      ("Rebecca","Rlpelkey@gmail.com","Wilson"),
      ("Michaely","mclancy91@yahoo.com","Jax"),
      ("Charla","Charla.coffin@att.net","Cherry"),
      ("Traci","Trishbolick@knology.net","Cece"),
      ("Jennie","Jennieskrobisz@gmail.com","CARBON-PIEDMONT"),
      ("Amanda","amp138k9@gmail.com","Luna"),
      ("Jacob","jacobscruggs1213@gmail.com","MAYA-SKY"),
      ("Paula","Paulag1@comcast.net","2Cooper"),
      ("Jessica","Jessica.N.berg@gmail.com","Tyson"),
      ("Rebecca","rebeccahall22@outlook.com","Dixie"),
      ("Summer","Summerkrouse@comcast.net","Sullivan"),
      ("Christi","C.e.snider2@gmail.com","Beauregard"),
      ("Bryn","bryntowner@gmail.com","Annie"),
      ("Regina","Trprice1989@comcast.net","Rocky"),
      ("Lauren","Lalanichole@gmail.com","Fitz"),
      ("Scottie","Scottiemcabee816@gmail.com","DIPPY-BAILEY"),
      ("Holly","Mobilevet@eastviewanimalwellness.com","Doc"),
      ("David","Jdlogue13@gmail.com","Gus"),
      ("Cherish","Brassknucklepinupsrescue@gmail.com","Felon"),
      ("Belinda","Belindahoward45@comcast.net","COPPER-DODGE"),
      ("Anne","Ahsidey@comcast.net","Sammie"),
      ("Mandy","Tonymandy@bellsouth.net","Samson"),
      ("Ashley","Ashleymac7241@gmail.com","DARCY-BAILEY"),
      ("Betsy","Blwestover@aol.com","FARLEY-MATILDA"),
      ("Katie","Kt20soccer@yahoo.com","DEXTER-BAILEY"),
      ("Margo","m.lozito10@comcast.net","Willie"),
      ("Patrick","Patrick@augustasportscouncil.com","Horace"),
      ("Kathy","Elliskk@comcast.net","Lucy")
    )

    if (Props.mode == Props.RunModes.Production) {
      nameEmailDog.map { case (name, email, dogName) =>
        EmailActor ! Send5kEmail(name, email, dogName)
      }
    }
  }
} 
