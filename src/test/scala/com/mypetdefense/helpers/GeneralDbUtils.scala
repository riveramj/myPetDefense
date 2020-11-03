package com.mypetdefense.helpers

import com.mypetdefense.generator._
import com.mypetdefense.helpers.db.AgencyDbUtils.createAgency
import com.mypetdefense.helpers.db.ShipmentDbUtils.createShipment
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.PetDbUtils.createPet
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.helpers.models.PetlandAndMPDAgencies
import com.mypetdefense.model._
import net.liftweb.common._

object GeneralDbUtils {

  val tppAgencyName     = "TPP"
  val mpdAgencyName     = "My Pet Defense"
  val petLandAgencyName = "Petland"

  case class InsertedUserAndSub(user: User, subscription: Subscription)

  case class InsertedUserAndPet(user: User, pets: List[Pet])

  case class InsertedUserSubAndShipment(
      user: User,
      subscription: Subscription,
      shipments: List[Shipment]
  )

  case class InsertedPetsUserSubAndShipment(
      user: User,
      subscription: Subscription,
      shipments: List[Shipment],
      pets: List[Pet]
  )

  def clearTables(): Unit = {
    Address.findAll().map(_.delete_!)
    Agency.findAll().map(_.delete_!)
    Event.findAll().map(_.delete_!)
    Pet.findAll().map(_.delete_!)
    Insert.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
    SubscriptionBox.findAll().map(_.delete_!)
    SubscriptionItem.findAll().map(_.delete_!)
    Shipment.findAll().map(_.delete_!)
    ShipmentLineItem.findAll().map(_.delete_!)
    User.findAll().map(_.delete_!)
  }

  def insertUserAndSub(
      uIn: UserCreateGeneratedData,
      sIn: SubscriptionCreateGeneratedData
  ): InsertedUserAndSub = {
    val u  = createUser(uIn)
    val s  = createSubscription(Full(u), sIn)
    val uU = u.subscription(s).saveMe()
    InsertedUserAndSub(uU, s)
  }

  def insertSubWithoutUser(sIn: SubscriptionCreateGeneratedData): Subscription =
    createSubscription(Empty, sIn)

  def insertUserSubAndShipment(in: ShipmentChainData): InsertedUserSubAndShipment = {
    val u        = createUser(in.user)
    val su       = createSubscription(Full(u), in.subscriptionCreateGeneratedData)
    val updatedU = u.subscription(su).saveMe()
    val shs      = in.shipmentCreateGeneratedData.map(createShipment(u, su, _))
    InsertedUserSubAndShipment(updatedU, su, shs)
  }

  def insertUserAndPet(in: PetChainData): InsertedUserAndPet = {
    val u = createUser(in.user)
    val p = in.petData.map(createPet(u, _))
    InsertedUserAndPet(u, p)
  }

  def insertPetsAndShipmentChainData(
      in: PetsAndShipmentChainData,
      inserts: List[Insert] = List.empty[Insert]
  ): InsertedPetsUserSubAndShipment = {
    val u           = createUser(in.user)
    val su          = createSubscription(Full(u), in.subscriptionCreateGeneratedData)
    val updatedUser = u.subscription(su).saveMe().refresh.toOption.get
    val pets = in.pets.map { pData =>
      val createdPet = createPet(u, pData)
      SubscriptionBox.createNewBox(su, createdPet)
      createdPet
    }
    val uSubscription = su.refresh.toOption.get
    val shs =
      in.shipmentCreateGeneratedData.map(createShipment(updatedUser, uSubscription, _, inserts))
    InsertedPetsUserSubAndShipment(updatedUser, uSubscription, shs, pets)
  }

  def insertPetsAndShipmentData(
      data: List[PetsAndShipmentChainData],
      agency: Agency,
      subUpgraded: Boolean
  ): List[InsertedPetsUserSubAndShipment] =
    data.map(insertPetAndShipmentsChainAtAgency(_, agency, subUpgraded))

  def insertPetAndShipmentsChainAtAgency(
      data: PetsAndShipmentChainData,
      agency: Agency,
      subUpgraded: Boolean,
      inserts: List[Insert] = List.empty[Insert]
  ): InsertedPetsUserSubAndShipment = {
    val inserted    = insertPetsAndShipmentChainData(data, inserts)
    val updatedUser = inserted.user.referer(agency).saveMe()
    val updatedSub  = inserted.subscription.isUpgraded(subUpgraded).saveMe()
    inserted.copy(user = updatedUser, subscription = updatedSub)
  }

  def createPetlandAndMPDAgencies(): PetlandAndMPDAgencies = {
    val myPetDefenseAgency = createAgency(mpdAgencyName)
    val petlandAgency      = createAgency(petLandAgencyName)
    PetlandAndMPDAgencies(petlandAgency, myPetDefenseAgency)
  }

}
