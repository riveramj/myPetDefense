package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.model.InventoryChangeAudit

class InventoryServiceSpec extends DBTest {

  it should "update item count" in {
    forAllNoShrink(listOfNInventoryItemsGen(), listOfNPosIntsGen()) { (items, newCounts) =>
      items foreach (_.saveMe())

      (items zip newCounts) foreach {
        case (item, newCount) =>
          InventoryService.updateItemCount(item, item.total.get, newCount)
      }

      val expectedIdsAndCounts = items.map(_.id.get) zip newCounts
      val actualIdsAndCounts = InventoryChangeAudit
        .findAll()
        .map(
          _.inventoryItem
            .map(item => (item.id, item.total))
            .openOrThrowException("Should not happen")
        )

      actualIdsAndCounts mustBe expectedIdsAndCounts

      cleanUpSuccess()
    }
  }

}
