package com.fleaTick.model

import net.liftweb.mapper._
import java.util.Date

class Admin extends LongKeyedMapper[Admin] with IdPK with OneToMany[Long, Admin] {
  def getSingleton = Admin
  object adminId extends MappedLong(this){
    override def dbIndexed_? = true
  }
  object user extends MappedLongForeignKey(this, User)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }
}

object Admin extends Admin with LongKeyedMetaMapper[Admin]
