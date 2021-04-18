package com.mypetdefense.helpers

import com.mypetdefense.generator._
import com.mypetdefense.helpers.BootUtil.testDatabase
import com.mypetdefense.helpers.db.AgencyDbUtils.createAgency
import com.mypetdefense.helpers.db.PetDbUtils.createPet
import com.mypetdefense.helpers.db.ProductDbUtils.createNewProduct
import com.mypetdefense.helpers.db.ProductScheduleDbUtils
import com.mypetdefense.helpers.db.ShipmentDbUtils.createShipment
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.SubscriptionUpgradeDbUtil.createSubscriptionUpgrade
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.helpers.models.{PetlandAndMPDAgencies, TppAndMPDAgencies}
import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.mapper.{DB, DefaultConnectionIdentifier}

object GeneralDbUtils {

  val tppAgencyName     = "TPP"
  val mpdAgencyName     = "My Pet Defense"
  val petLandAgencyName = "Petland"

  case class InsertedUserAndSub(user: User, subscription: Subscription)

  case class InsertedUserSubPetsUpgradeCount(
    user: User,
    subscription: Subscription,
    pets: List[Pet],
    countAtUpgrade: Int
)

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

  case class InsertedScheduleAndProduct(
      schedule: ProductSchedule,
      products: List[Product]
  )

  val insertUserAndSubTupled
      : ((UserCreateGeneratedData, SubscriptionCreateGeneratedData)) => InsertedUserAndSub =
    (insertUserAndSub _).tupled

  def clearTables(): Unit = {
    val tables = List(
      Address,
      Agency,
      Event,
      Pet,
      Insert,
      Subscription,
      SubscriptionBox,
      SubscriptionItem,
      Shipment,
      ShipmentLineItem,
      ProductSchedule,
      ProductScheduleItem,
      Product,
      User,
      ActionLog,
      ActionLogDetails,
      SubscriptionUpgrade,
    )

    val truncateAllQuery = testDatabase.truncateAllQuery(tables.map(_.dbTableName))

    DB.use(DefaultConnectionIdentifier) { conn =>
      DB.prepareStatement(truncateAllQuery, conn) { _.executeUpdate() }
    }
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
      inserts: List[Insert] = List.empty[Insert],
      upgraded: Boolean = false
  ): InsertedPetsUserSubAndShipment = {
    val u           = createUser(in.user)
    val su          = createSubscription(Full(u), in.subscriptionCreateGeneratedData)
    val updatedUser = u.subscription(su).saveMe().reload
    val pets = in.pets.map { pData =>
      val createdPet = createPet(u, pData)
      SubscriptionBox.createNewBox(su, createdPet, upgraded)
      createdPet
    }
    val uSubscription = su.reload
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
    val inserted    = insertPetsAndShipmentChainData(data, inserts, subUpgraded)
    val updatedUser = inserted.user.referer(agency).saveMe()
    val updatedSub  = inserted.subscription.isUpgraded(subUpgraded).saveMe()
    inserted.copy(user = updatedUser, subscription = updatedSub)
  }

  def insertSubscriptionUpgrade(
    upgradeData: SubscriptionUpgradeCreateGeneratedData,
    user: User,
    subscription: Subscription
  ) =
    createSubscriptionUpgrade(
      upgradeData,
      user,
      subscription
    )

  def insertPetsAndUserData(
    data: List[PetData],
    user: User
  ): List[Pet] =
    data.map(insertPetWithUserId(_, user))

  def insertPetWithUserId(pet: PetData, user: User) =
    createPet(user, pet)

  def insertProductScheduleGeneratedData(
      in: ProductScheduleGeneratedChainData
  ): InsertedScheduleAndProduct = {
    val insertedProducts = in.productData.map(createNewProduct)
    val schedule =
      ProductScheduleDbUtils.createProductSchedule(in.scheduleStartData, insertedProducts)
    InsertedScheduleAndProduct(schedule, insertedProducts)
  }

  def createPetlandAndMPDAgencies(): PetlandAndMPDAgencies = {
    val myPetDefenseAgency = createAgency(mpdAgencyName)
    val petlandAgency      = createAgency(petLandAgencyName)
    PetlandAndMPDAgencies(petlandAgency, myPetDefenseAgency)
  }

  def createTppAndMPDAgencies(): TppAndMPDAgencies = {
    val myPetDefenseAgency = createAgency(mpdAgencyName)
    val tppAgency      = createAgency(tppAgencyName)
    TppAndMPDAgencies(tppAgency, myPetDefenseAgency)
  }

  def insertProductToToSubBoxes(subscription: Subscription, product: Product): Unit =
    subscription.subscriptionBoxes.toList
      .foreach(SubscriptionItem.createSubscriptionItem(product, _))

}
