package com.mypetdefense.jobs

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.ProductDbUtils.createNewProduct
import com.mypetdefense.model._
import net.liftweb.common.{Empty, Full}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SubscriptionBoxProductsUpdateJobSpec extends DBTest {
  it should "do regular schedule job properly" in {
    val mpdAndPld                      = createPetlandAndMPDAgencies()
    val someGeneratedProduct           = product()
    val someOldInsertedProduct         = createNewProduct(someGeneratedProduct)
    val scheduleGeneratedData          = productUpdateSchedule(productsSize = 3)
    val previousScheduleGeneratedData  = productUpdateSchedule(productsSize = 3)
    val insertedProductsUpdateSchedule = insertProductScheduleGeneratedData(scheduleGeneratedData)
    val insertedPreviousSchedule =
      insertProductScheduleGeneratedData(previousScheduleGeneratedData).schedule
        .scheduleStatus(ProductScheduleStatus.Active)
        .saveMe()
    val userAndPetShouldBeAffected    = petsAndShipmentChainDataGen()
    val userAndPetShouldNotBeAffected = petsAndShipmentChainDataGen()
    val shouldBeAffectedData =
      insertPetAndShipmentsChainAtAgency(
        userAndPetShouldBeAffected,
        mpdAndPld.mpd,
        subUpgraded = true
      )
    val shouldNotBeAffectedData =
      insertPetAndShipmentsChainAtAgency(
        userAndPetShouldNotBeAffected,
        mpdAndPld.mpd,
        subUpgraded = true
      )

    shouldNotBeAffectedData.subscription.subscriptionBoxes.toList
      .map(_.userModified(true).saveMe())
      .map(SubscriptionItem.createSubscriptionItem(someOldInsertedProduct, _))

    val job = new SubscriptionBoxProductsUpdateJob
    job.executeNextOrActiveRegularSchedule(
      Full(insertedProductsUpdateSchedule.schedule),
      Full(insertedPreviousSchedule)
    )
    val productsInSubsBoxesUpdateIds = insertedProductsUpdateSchedule.products.map(_.id.get)

    val allSubsItems           = SubscriptionItem.findAll
    val shouldBeNewProductsIds = getProducts(shouldBeAffectedData).map(_.id.get)
    val shouldBeOldProductsIds = getProducts(shouldNotBeAffectedData).map(_.id.get)

    allSubsItems.size shouldBe (shouldBeNewProductsIds.size + shouldBeOldProductsIds.size)
    shouldBeOldProductsIds.toSet should contain theSameElementsAs List(
      someOldInsertedProduct.id.get
    )
    shouldBeNewProductsIds.toSet should contain theSameElementsAs productsInSubsBoxesUpdateIds
    insertedPreviousSchedule.scheduleStatus.get shouldBe ProductScheduleStatus.Completed
  }

  it should "do first box schedule job properly" in {
    val scheduleGeneratedData          = productUpdateSchedule(productsSize = 3)
    val previousScheduleGeneratedData  = productUpdateSchedule(productsSize = 3)
    val insertedProductsUpdateSchedule = insertProductScheduleGeneratedData(scheduleGeneratedData)
    val insertedPreviousSchedule =
      insertProductScheduleGeneratedData(previousScheduleGeneratedData).schedule
        .firstBox(true)
        .scheduleStatus(ProductScheduleStatus.Active)
        .saveMe()
    insertedProductsUpdateSchedule.schedule.firstBox(true).saveMe()
    val expectedProductsIds = insertedProductsUpdateSchedule.products.map(_.id.get)

    val job = new SubscriptionBoxProductsUpdateJob
    job.executeFirstBoxSchedule(
      Full(insertedProductsUpdateSchedule.schedule),
      Full(insertedPreviousSchedule)
    )

    val firstBoxProductsIds = ProductSchedule.getFirstBoxProducts.map(_.id.get)
    firstBoxProductsIds should contain theSameElementsAs expectedProductsIds
  }

  it should "run previous job only for yesterdays subs" in {
    val mpdAndPld                      = createPetlandAndMPDAgencies()
    val someGeneratedProduct           = product()
    val someOldInsertedProduct         = createNewProduct(someGeneratedProduct)
    val scheduleGeneratedData          = productUpdateSchedule(productsSize = 3)
    val insertedProductsUpdateSchedule = insertProductScheduleGeneratedData(scheduleGeneratedData)
    val userAndPetShouldBeAffected     = petsAndShipmentChainDataGen()
    val userAndPetShouldNotBeAffected  = petsAndShipmentChainDataGen()
    val insertedPreviousSchedule =
      insertedProductsUpdateSchedule.schedule
        .scheduleStatus(ProductScheduleStatus.Active)
        .saveMe()
    val shouldBeAffectedData =
      insertPetAndShipmentsChainAtAgency(
        userAndPetShouldBeAffected,
        mpdAndPld.mpd,
        subUpgraded = true
      )
    val shouldNotBeAffectedData =
      insertPetAndShipmentsChainAtAgency(
        userAndPetShouldNotBeAffected,
        mpdAndPld.mpd,
        subUpgraded = true
      )
    shouldBeAffectedData.subscription.createdAt(yesterday).saveMe()
    shouldNotBeAffectedData.subscription.createdAt(anyDayExceptYesterday).saveMe()
    insertProductToToSubBoxes(shouldBeAffectedData.subscription, someOldInsertedProduct)
    insertProductToToSubBoxes(shouldNotBeAffectedData.subscription, someOldInsertedProduct)

    val job = new SubscriptionBoxProductsUpdateJob
    job.executeNextOrActiveRegularSchedule(
      Empty,
      Full(insertedPreviousSchedule)
    )

    val allSubsItems                 = SubscriptionItem.findAll
    val shouldBeNewProductsIds       = getProducts(shouldBeAffectedData).map(_.id.get)
    val shouldBeOldProductsIds       = getProducts(shouldNotBeAffectedData).map(_.id.get)
    val productsInSubsBoxesUpdateIds = insertedProductsUpdateSchedule.products.map(_.id.get)
    val schedulers                   = ProductSchedule.findAll()

    allSubsItems.size shouldBe (shouldBeNewProductsIds.size + shouldBeOldProductsIds.size)
    schedulers.size shouldBe 1
    schedulers.head.scheduleStatus.get shouldBe ProductScheduleStatus.Active
    shouldBeNewProductsIds.toSet should contain theSameElementsAs productsInSubsBoxesUpdateIds
  }

  private def getProducts(in: InsertedPetsUserSubAndShipment): List[Product] =
    in.subscription.subscriptionBoxes
      .map(_.reload)
      .flatMap(_.subscriptionItems.toList)
      .flatMap(_.product.toList)
      .toList

  private def insertProductToToSubBoxes(subscription: Subscription, product: Product): Unit =
    subscription.subscriptionBoxes.toList
      .foreach(SubscriptionItem.createSubscriptionItem(product, _))

}
