package com.mypetdefense.helpers.db

import com.mypetdefense.generator.InsertGenData
import com.mypetdefense.model.Insert

object InsertsDbHelper {
  def createInsert(in: InsertGenData): Insert =
    Insert.createNewInsert(in.name, in.itemNumber.toString, in.weight)
}
