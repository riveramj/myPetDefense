package com.fleaTick.model

import net.liftweb.mapper._

class Admin extends LongKeyedMapper[Admin] with IdPK with OneToMany[Long, Admin] {
  def getSingleton = Admin
  object adminId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
}

object Admin extends Admin with LongKeyedMetaMapper[Admin] {
  override def fieldOrder = List(adminId)
}
