package com.mypetdefense.util.csv

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.mypetdefense.util.DateHelper.now
import net.liftweb.common._

final case class CustomerInfo(
    email: String,
    firstName: String,
    lastName: String,
    street1: String,
    street2: Option[String],
    city: String,
    state: String,
    zip: String,
    phone: String
)

final case class PetInfo(
    petName: String,
    size: String,
    breed: String,
    birthday: LocalDate
)

final case class CustomerAndPetsInfo(
    customer: CustomerInfo,
    pet1: PetInfo,
    pet2: Option[PetInfo]
)

object MissingPetsUploadCSV extends GenericCSVParser[CustomerAndPetsInfo] {
  override def convertRowToModel(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[CustomerAndPetsInfo] = {
    import Columns._

    val email     = cellValue(Email, headerIndex, fieldList).openOr("")
    val firstName = cellValue(FirstName, headerIndex, fieldList).openOr("")
    val lastName  = cellValue(LastName, headerIndex, fieldList).openOr("")
    val street1   = cellValue(Street1, headerIndex, fieldList).openOr("")
    val street2   = cellValue(Street2, headerIndex, fieldList)
    val city      = cellValue(City, headerIndex, fieldList).openOr("")
    val state     = cellValue(State, headerIndex, fieldList).openOr("")
    val zip       = cellValue(Zip, headerIndex, fieldList).openOr("")
    val phone     = cellValue(Phone, headerIndex, fieldList).openOr("")

    val pet1Name     = cellValue(Pet1Name, headerIndex, fieldList).openOr("")
    val pet1Size     = cellValue(Pet1Size, headerIndex, fieldList).openOr("")
    val pet1Breed    = cellValue(Pet1Breed, headerIndex, fieldList).openOr("")
    val pet1Birthday = cellLocalDate(Pet1Birthday, headerIndex, fieldList).openOr(DefaultBirthday)

    val pet2Name     = cellValue(Pet2Name, headerIndex, fieldList)
    val pet2Size     = cellValue(Pet2Size, headerIndex, fieldList)
    val pet2Breed    = cellValue(Pet2Breed, headerIndex, fieldList)
    val pet2Birthday = cellLocalDate(Pet2Birthday, headerIndex, fieldList)

    val customer =
      CustomerInfo(email, firstName, lastName, street1, street2, city, state, zip, phone)
    val pet1 = PetInfo(pet1Name, pet1Size, pet1Breed, pet1Birthday)
    val pet2 =
      (pet2Name zip pet2Size zip pet2Breed zip pet2Birthday).map {
        case (((n, s), b), d) => PetInfo(n, s, b, d)
      }.headOption

    Full(CustomerAndPetsInfo(customer, pet1, pet2))
  }

  object Columns extends Columns {
    val Email: HeaderValue     = requiredValue("email")
    val FirstName: HeaderValue = requiredValue("first_name")
    val LastName: HeaderValue  = requiredValue("last_name")
    val Street1: HeaderValue   = requiredValue("street1")
    val Street2: HeaderValue   = requiredValue("street2")
    val City: HeaderValue      = requiredValue("city")
    val State: HeaderValue     = requiredValue("state")
    val Zip: HeaderValue       = requiredValue("zip")
    val Phone: HeaderValue     = requiredValue("phone")

    val Pet1Name: HeaderValue     = requiredValue("pet1_name")
    val Pet1Size: HeaderValue     = requiredValue("pet1_size")
    val Pet1Breed: HeaderValue    = requiredValue("pet1_breed")
    val Pet1Birthday: HeaderValue = requiredValue("pet1_birthday")

    val Pet2Name: HeaderValue     = requiredValue("pet2_name")
    val Pet2Size: HeaderValue     = requiredValue("pet2_size")
    val Pet2Breed: HeaderValue    = requiredValue("pet2_breed")
    val Pet2Birthday: HeaderValue = requiredValue("pet2_birthday")

    val DefaultBirthday: LocalDate = now

    private val DateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE

    def cellLocalDate(
        column: HeaderValue,
        headerIndex: Map[Value, Int],
        row: Array[String]
    ): Box[LocalDate] =
      cellValue(column, headerIndex, row).flatMap { date =>
        Box.tryo(LocalDate.parse(date, DateFormatter))
      }
  }
}
