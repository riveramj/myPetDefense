package com.mypetdefense.jobs

import org.quartz._

import net.liftweb._
import common._
import java.io.{StringWriter, PrintWriter}

/**
  * Extend jobs from this to get a try/catch block and logging.
  */
trait ManagedJob extends Job with Loggable {
  protected def executeOp(context: JobExecutionContext)(func: => Unit): Unit = {
    val fireId = context.getFireInstanceId
    val key = context.getJobDetail.getKey
    MDC.put(("fireInstanceId", fireId))
    MDC.put(("jobKey", key.toString))
    try {
      logger.debug("Job started.")
      func
      logger.debug("Job finished.")
    }
    catch {
      case e: Exception =>
        val result = new StringWriter
        val printWriter = new PrintWriter(result)
        e.printStackTrace(printWriter)
        logger.error(s"Exception running job $fireId: ${result.toString}")
    }

    MDC.clear() // not sure this is necessary
  }
}

trait TriggeredJob {
  def detail: JobDetail
  def trigger: Trigger
}

