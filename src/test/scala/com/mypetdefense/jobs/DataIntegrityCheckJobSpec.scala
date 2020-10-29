package com.mypetdefense.jobs

import java.util.Date

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.{DBTest, Random}
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.model.{Event, Pet, Shipment, SubscriptionBox, User}
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

        val allEvents              = Event.findAll()
        val actualProblemShipments = allEvents.flatMap(_.shipment.toList)
        val actualPets             = allEvents.flatMap(_.pet.toList)
        val actualSubs             = allEvents.flatMap(_.subscription.toList)
        val actualUsers            = allEvents.flatMap(_.user.toList).toSet
        actualProblemShipments should contain theSameElementsAs expectedShipments
        actualPets should contain theSameElementsAs expectedPets
        actualSubs should contain theSameElementsAs expectedSubs
        actualUsers should contain theSameElementsAs expectedUsers

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
