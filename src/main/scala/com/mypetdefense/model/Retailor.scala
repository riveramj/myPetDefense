package com.mypetdefense.model

import net.liftweb._
  import mapper._
  import common._
  import util._

import com.mypetdefense.util.RandomIdGenerator._

import java.util.Date

class Agent extends LongKeyedMapper[Agent] with IdPK {
  def getSingleton = Agent
  object agentId extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object name extends MappedString(this, 100)
  object member extends MappedLongForeignKey(this, User)
  object createdAt extends MappedDateTime(this) {
    override def defaultValue = new Date()
  }

  def createNewAgent(
    name: String, 
    agent: User
  ) = {
    Agent.create
    .agentId(generateLongId)
    .name(name)
    .member(agent)
    .saveMe
  }
}

object Agent extends Agent with LongKeyedMetaMapper[Agent]


