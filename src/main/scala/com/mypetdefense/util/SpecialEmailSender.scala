package com.mypetdefense.util 

import com.mypetdefense.model._
import net.liftweb._
  import common._
  import util._
import net.liftweb.mapper.By
import me.frmr.stripe.{Coupon => StripeCoupon, Subscription => _}
import dispatch._, Defaults._

object SpecialEmailSender extends Loggable {
  def send5kEmails = {
    val nameEmailDog = List(
      ("Chelsea","chenson724@comcast.net","Connoer and Dalton"),
      ("Cindy","cbair59@comcast.net","Bo"),
      ("Brooke","Brookerobinson95@gmail.com","Bacon"),
      ("Todd","Taprest@comcast.net","Sookie"),
      ("Ashley","Ashleyprescott90@gmail.com","Zoey grace"),
      ("Leah","doodlebug_20@hotmail.com","Jerry"),
      ("Leah","doodlebug_20@hotmail.com","Penelope"),
      ("Janise","Janise_jordan@yahoo.com","Hope"),
      ("Wade","wade.cotton01@gmail.com","Cooper"),
      ("Cecelia","Ceceliaamorgan@gmail.com","Marlowe"),
      ("Julie","Karnoscakj@yahoo.com","Sadie"),
      ("Rhonda","Rhonda.cotton01@gmail.com","Jack"),
      ("Mara","Jjust1@bellsouth.net","Lucylu Scarlett"),
      ("David","Davidandkate.phillips@gmail.com","Leber"),
      ("Katie","Katie.h.tracy@gmail.com","2 Kids"),
      ("Megan","Megan.l.manly@mac.com","zeke"),
      ("Stephanie","Scarani@columbiacountyso.org","Loki"),
      ("Megan","meganwinzeler@gmail.com","Cairo"),
      ("Amanda","Amandalhall@yahoo.com","Sam"),
      ("Kara","Stymie09@comcast.net","Ruby, Dixie, Piper"),
      ("Allyn","Amccann4131@gmail.com","Sage"),
      ("Jesi","jesi.cotton5@yahoo.com","Baxter"),
      ("Rebecca","Rlpelkey@gmail.com","Wilson"),
      ("Michaely","mclancy91@yahoo.com","Jax"),
      ("Charla","Charla.coffin@att.net","Cherry"),
      ("Traci","Trishbolick@knology.net","Cece"),
      ("Jennie","Jennieskrobisz@gmail.com","Carbon Piedmont"),
      ("Amanda","amp138k9@gmail.com","Luna"),
      ("Jacob","jacobscruggs1213@gmail.com","Skye and Maya"),
      ("Paula","Paulag1@comcast.net","Cooper"),
      ("Jessica","Jessica.N.berg@gmail.com","Tyson"),
      ("Rebecca","rebeccahall22@outlook.com","Dixie"),
      ("Summer","Summerkrouse@comcast.net","Sullivan"),
      ("Christi","C.e.snider2@gmail.com","Beauregard"),
      ("Bryn","bryntowner@gmail.com","Annie"),
      ("Regina","Trprice1989@comcast.net","Rocky"),
      ("Lauren","Lalanichole@gmail.com","Fitz"),
      ("Scottie","Scottiemcabee816@gmail.com","Dippy & Bailey"),
      ("Holly","Mobilevet@eastviewanimalwellness.com","Doc"),
      ("David","Jdlogue13@gmail.com","Gus"),
      ("Cherish","Brassknucklepinupsrescue@gmail.com","Felon"),
      ("Belinda","Belindahoward45@comcast.net","Copper and Dodge"),
      ("Anne","Ahsidey@comcast.net","Sammie"),
      ("Mandy","Tonymandy@bellsouth.net","Samson"),
      ("Ashley","Ashleymac7241@gmail.com","Bailey"),
      ("Betsy","Blwestover@aol.com","Matilda and Farley"),
      ("Amanda","amp138k9@gmail.com","Freyja"),
      ("Katie","Kt20soccer@yahoo.com","Bai"),
      ("Margo","m.lozito10@comcast.net","Willie"),
      ("Patrick","Patrick@augustasportscouncil.com","Horace"),
      ("Kathy","Elliskk@comcast.net","Lucy")
    )


  }
} 
