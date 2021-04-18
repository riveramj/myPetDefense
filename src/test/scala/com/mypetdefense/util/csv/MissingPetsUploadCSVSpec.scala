package com.mypetdefense.util.csv

import java.time.LocalDate

import net.liftweb.common.Full

class MissingPetsUploadCSVSpec extends GenericCSVParserSpec[CustomerAndPetsInfo] {

  override val parser = MissingPetsUploadCSV

  it should "properly parse MissingPetsUploadCSV" in {
    val csv =
      """email,first_name,last_name,street1,street2,city,state,zip,phone,pet1_name,pet1_size,pet1_breed,pet1_birthday,pet2_name,pet2_size,pet2_breed,pet2_birthday
        |foo@gmail.com,john,doe,123 main st,,atl,ga,30332,123-123-1234,Spot,small,pitbull,2020-11-29,Doge,big,amstaff,2020-12-10""".stripMargin

    csv mustBeParsedTo
      Full(
        List(
          CustomerAndPetsInfo(
            CustomerInfo(
              "foo@gmail.com",
              "john",
              "doe",
              "123 main st",
              None,
              "atl",
              "ga",
              "30332",
              "123-123-1234"
            ),
            PetInfo("Spot", "small", "pitbull", LocalDate.of(2020, 11, 29)),
            Some(PetInfo("Doge", "big", "amstaff", LocalDate.of(2020, 12, 10)))
          )
        )
      )
  }

  it should "properly parse MissingPetsUploadCSV with missing second pet" in {
    val csv =
      """email,first_name,last_name,street1,street2,city,state,zip,phone,pet1_name,pet1_size,pet1_breed,pet1_birthday,pet2_name,pet2_size,pet2_breed,pet2_birthday
        |foo@gmail.com,john,doe,123 main st,,atl,ga,30332,123-123-1234,Spot,small,pitbull,2020-11-29,,,,""".stripMargin

    csv mustBeParsedTo
      Full(
        List(
          CustomerAndPetsInfo(
            CustomerInfo(
              "foo@gmail.com",
              "john",
              "doe",
              "123 main st",
              None,
              "atl",
              "ga",
              "30332",
              "123-123-1234"
            ),
            PetInfo("Spot", "small", "pitbull", LocalDate.of(2020, 11, 29)),
            None
          )
        )
      )
  }
}
