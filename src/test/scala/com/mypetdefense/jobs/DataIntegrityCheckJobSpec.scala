package com.mypetdefense.jobs

import java.util.Date

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.{DBTest, Random}
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.model.{Event, EventType, Pet, Shipment, SubscriptionBox, User}
import net.liftweb.common.Full
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DataIntegrityCheckJobSpec extends DBTest {

  private val threeDaysAgoDate: Date = threeDaysAgo.toDate

  it should "properly check data integrity" in {
    forAll(
      listOfNUsersGen(3),
      listOfSubscriptionToCreateGen(3),
      listOfNPetsChainDataGen(3),
      listOfNPetsChainDataGen(1),
      listOfNShipmentChainDataGen(3),
      listOfNShipmentChainDataGen(1)
    ) {
      (
          usersWithoutSubs,
          subsWithoutUsers,
          petsWithoutBoxes,
          petsWithBoxes,
          oldUntraceableShipments,
          trackableOldShipments
      ) =>
        val insertedUsersWithoutSubs = usersWithoutSubs.map(createUser)
        val insertedSubsWithoutUsers = subsWithoutUsers.map(insertSubWithoutUser)
        val insertedPetsWithoutBoxes = petsWithoutBoxes.map(insertUserAndPet)
        val insertedOldShipmentsWithoutTrackingNumbers = oldUntraceableShipments
          .map(insertUserSubAndShipment)
          .flatMap(inserted => inserted.shipments.map(setProcessedDateToMoreThanThreeDaysAgo))
        petsWithBoxes.map(insertUserAndPet).map(createBoxAndSubscription)
        trackableOldShipments
          .map(insertUserSubAndShipment)
          .foreach(
            _.shipments.map(setProcessedDateToMoreThanThreeDaysAgo).map(setRandomTrackingNumber)
          )

        new DataIntegrityCheckJob().checkDataIntegrity()

        val expectedShipments = insertedOldShipmentsWithoutTrackingNumbers
        val expectedPets      = insertedPetsWithoutBoxes.flatMap(_.pets)
        val expectedSubs = insertedSubsWithoutUsers ++ insertedOldShipmentsWithoutTrackingNumbers
          .flatMap(_.subscription.toList)
        val insertedWithoutTrackedNumberUsers = insertedOldShipmentsWithoutTrackingNumbers
          .flatMap(
            _.subscription.map(_.user.toList)
          )
          .flatten
        val expectedUsers =
          (insertedUsersWithoutSubs ++ insertedPetsWithoutBoxes.map(_.user) ++ insertedWithoutTrackedNumberUsers).toSet
        val expectedTitles = List(
          "Shipment doesn't have a tracking number for three days.",
          "Pet doesn't have a box",
          "Subscription doesn't have an owner",
          "User doesn't have a subscription"
        ).toSet
        val expectedDetails = List(
          "During regular data integrity job, that shipment was found, manual handling is needed.",
          "During regular data integrity job, that pet was found, manual handling is needed.",
          "During regular data integrity job, that subscription was found, manual handling is needed.",
          "During regular data integrity job, that user was found, manual handling is needed."
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
        actualEventTypes should contain theSameElementsAs actualEventTypes

        cleanUpSuccess()
    }
  }

  private def setProcessedDateToMoreThanThreeDaysAgo(in: Shipment): Shipment =
    in.dateProcessed(threeDaysAgoDate).saveMe()

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
