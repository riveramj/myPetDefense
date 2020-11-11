package com.mypetdefense.jobs

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers.db.ProductDbUtils.createNewProduct
import com.mypetdefense.model._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SubscriptionBoxProductsUpdateJobSpec extends DBTest {
  it should "update boxes properly" in {
    val mpdAndPld                      = createPetlandAndMPDAgencies()
    val someGeneratedProduct           = product()
    val someOldInsertedProduct         = createNewProduct(someGeneratedProduct)
    val scheduleGeneratedData          = productUpdateSchedule(productsSize = 3)
    val insertedProductsUpdateSchedule = insertProductScheduleGeneratedData(scheduleGeneratedData)
    val userAndPetShouldBeAffected     = petsAndShipmentChainDataGen()
    val userAndPetShouldNotBeAffected  = petsAndShipmentChainDataGen()
    val shouldBeAffectedData =
      insertPetAndShipmentsChainAtAgency(
        userAndPetShouldBeAffected,
        mpdAndPld.mpd,
        subUpgraded = false
      )
    val shouldNotBeAffectedData =
      insertPetAndShipmentsChainAtAgency(
        userAndPetShouldNotBeAffected,
        mpdAndPld.mpd,
        subUpgraded = false
      )

    shouldNotBeAffectedData.subscription.subscriptionBoxes.toList
      .map(_.userModified(true).saveMe())
      .map(SubscriptionItem.createSubscriptionItem(someOldInsertedProduct, _))

    val job = new SubscriptionBoxProductsUpdateJob
    job.updateSubscriptionBoxes(insertedProductsUpdateSchedule.schedule)
    val productsInSubsBoxesUpdateIds = insertedProductsUpdateSchedule.products.map(_.id.get)

    val allSubsItems           = SubscriptionItem.findAll
    val shouldBeNewProductsIds = getProducts(shouldBeAffectedData).map(_.id.get)
    val shouldBeOldProductsIds = getProducts(shouldNotBeAffectedData).map(_.id.get)

    allSubsItems.size shouldBe (shouldBeNewProductsIds.size + shouldBeOldProductsIds.size)
    shouldBeOldProductsIds.toSet should contain theSameElementsAs List(
      someOldInsertedProduct.id.get
    )
    shouldBeNewProductsIds.toSet should contain theSameElementsAs productsInSubsBoxesUpdateIds
  }

  private def getProducts(in: InsertedPetsUserSubAndShipment): List[Product] =
    in.subscription.subscriptionBoxes
      .map(_.reload)
      .flatMap(_.subscriptionItems.toList)
      .flatMap(_.product.toList)
      .toList

}
