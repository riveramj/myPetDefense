package com.mypetdefense.jobs

import java.util.Date

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.{DBTest, Random}
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.model.{Event, EventType, Pet, Shipment, Subscription, SubscriptionBox, User}
import net.liftweb.common.Full
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DataIntegrityCheckJobSpec extends DBTest {

  private val threeDaysAgoDate: Date = threeDaysAgo.toDate
  private val sixtyDaysAgo: Date     = anyDayOfThisYearUntilSixtyDaysAgo.toDate

  it should "properly check data integrity and don't make duplicates" in {
    forAll(
      listOfNUsersGen(2),
      listOfSubscriptionToCreateGen(2),
      listOfNPetsChainDataGen(2),
      listOfNPetsChainDataGen(1),
      listOfNShipmentChainDataGen(2),
      listOfNShipmentChainDataGen(1)
    ) {
      (
          usersWithoutSubs,
          subsWithoutUsers,
          petsWithoutBoxes,
          petsWithBoxes,
          oldUntraceableShipments,
          oldEmptyLineItemsShipments
      ) =>
        val insertedUsersWithoutSubs = usersWithoutSubs.map(createUser)
        val insertedSubsWithoutUsers = subsWithoutUsers.map(insertSubWithoutUser)
        val insertedPetsWithoutBoxes = petsWithoutBoxes.map(insertUserAndPet)
        val insertedOldShipmentsWithoutTrackingNumbers = oldUntraceableShipments
          .map(insertUserSubAndShipment)
          .flatMap(inserted => inserted.shipments.map(setProcessedDateToMoreThanThreeDaysAgo))
        petsWithBoxes.map(insertUserAndPet).map(createBoxAndSubscription)
        val insertedOldEmptyLineItemsShipments = oldEmptyLineItemsShipments
          .map(insertUserSubAndShipment)
          .flatMap(
            _.shipments
              .map(setProcessedDateToMoreThanSixtyDaysAgo)
              .map(setStatusLabelCreatedOrPaid)
              .map(setRandomTrackingNumber)
          )

        val job = new DataIntegrityCheckJob()
        job.checkDataIntegrity()
        job.checkDataIntegrity()

        val expectedShipments =
          insertedOldShipmentsWithoutTrackingNumbers ++ insertedOldEmptyLineItemsShipments
        val expectedPets = insertedPetsWithoutBoxes.flatMap(_.pets)
        val expectedSubs = getExpectedSubs(
          insertedSubsWithoutUsers,
          insertedOldShipmentsWithoutTrackingNumbers,
          insertedOldEmptyLineItemsShipments
        )
        val expectedUsers = getExpectedUsers(
          insertedUsersWithoutSubs,
          insertedPetsWithoutBoxes,
          insertedOldShipmentsWithoutTrackingNumbers,
          insertedOldEmptyLineItemsShipments
        )
        val expectedTitles = List(
          "Shipment doesn't have a tracking number for three days.",
          "Pet doesn't have a box",
          "Subscription doesn't have an owner",
          "User doesn't have a subscription",
          "Shipment doesn't have shipping line items for sixty days."
        ).toSet
        val expectedDetails = List(
          "During regular data integrity job, that shipment was found, manual handling is needed.",
          "During regular data integrity job, that pet was found, manual handling is needed.",
          "During regular data integrity job, that subscription was found, manual handling is needed.",
          "During regular data integrity job, that user was found, manual handling is needed.",
          "During regular data integrity job, that shipment was found, manual handling is needed."
        ).toSet
        val expectedEventTypes =
          List(EventType.Shipping, EventType.Pets, EventType.Subscription, EventType.User).toSet

        val allEvents              = Event.findAll()
        val actualProblemShipments = allEvents.flatMap(_.shipment.toList)
        val actualPets             = allEvents.flatMap(_.pet.toList)
        val actualSubs             = allEvents.flatMap(_.subscription.toList)
        val actualUsers            = allEvents.flatMap(_.user.toList).toSet
        val actualTitles           = allEvents.map(_.title.get).toSet
        val actualDetails          = allEvents.map(_.details.get).toSet
        val actualEventTypes       = allEvents.map(_.eventType.get).toSet
        actualProblemShipments should contain theSameElementsAs expectedShipments
        actualPets should contain theSameElementsAs expectedPets
        actualSubs should contain theSameElementsAs expectedSubs
        actualUsers should contain theSameElementsAs expectedUsers
        actualTitles should contain theSameElementsAs expectedTitles
        actualDetails should contain theSameElementsAs expectedDetails
        actualEventTypes should contain theSameElementsAs expectedEventTypes

        cleanUpSuccess()
    }
  }

  private def getExpectedUsers(
      insertedUsersWithoutSubs: List[User],
      insertedPetsWithoutBoxes: List[InsertedUserAndPet],
      insertedOldShipmentsWTNumbers: List[Shipment],
      insertedOldEmptyLineItemsShipments: List[Shipment]
  ): Set[User] = {
    val insertedWithoutTrackedNumberUsers = getUsersOfShipments(insertedOldShipmentsWTNumbers)
    val insertedOELIShipmentsUsers        = getUsersOfShipments(insertedOldEmptyLineItemsShipments)
    (insertedUsersWithoutSubs ++ insertedPetsWithoutBoxes.map(_.user) ++ insertedWithoutTrackedNumberUsers ++ insertedOELIShipmentsUsers).toSet
  }

  private def getExpectedSubs(
      insertedSubsWithoutUsers: List[Subscription],
      insertedOldShipmentsWTNumbers: List[Shipment],
      insertedOldEmptyLineItemsShipments: List[Shipment]
  ): List[Subscription] = {
    val insertedOldShipmentsWTNumbersSubs = getSubsOfShipments(insertedOldShipmentsWTNumbers)
    val insertedOldEmptyLineItemsSubs     = getSubsOfShipments(insertedOldEmptyLineItemsShipments)
    insertedSubsWithoutUsers ++ insertedOldShipmentsWTNumbersSubs ++ insertedOldEmptyLineItemsSubs
  }

  private def getSubsOfShipments(in: List[Shipment]): List[Subscription] =
    in.flatMap(_.subscription.toList)

  private def getUsersOfShipments(in: List[Shipment]): List[User] =
    in.flatMap(
        _.subscription.map(_.user.toList)
      )
      .flatten

  private def setProcessedDateToMoreThanThreeDaysAgo(in: Shipment): Shipment =
    in.dateProcessed(threeDaysAgoDate).saveMe()

  private def setProcessedDateToMoreThanSixtyDaysAgo(in: Shipment): Shipment =
    in.dateProcessed(sixtyDaysAgo).saveMe()

  private def setStatusLabelCreatedOrPaid(in: Shipment): Shipment =
    in.shipmentStatus(statusLabelCreatedOrPaid()).saveMe()

  private def setRandomTrackingNumber(in: Shipment): Shipment =
    in.trackingNumber(Random.generateString.take(10)).saveMe()

  private def createBoxAndSubscription(inserted: InsertedUserAndPet): User = {
    val sub = createSubscription(Full(inserted.user), subscription())
    inserted.pets.foreach(createBoxForPet)
    inserted.user.subscription(sub).saveMe()
  }

  private def createBoxForPet(pet: Pet): SubscriptionBox = {
    val box = SubscriptionBox.create.pet(pet).saveMe()
    pet.box(box).saveMe()
    box
  }

}
