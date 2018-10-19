package com.mypetdefense.jobs

import org.quartz.impl.StdSchedulerFactory

import net.liftweb._
import common._
import util._

object JobManager extends SimpleInjector with Loggable {

  val jobs = new Inject(() => calcJobs) {}

  // Quartz Scheduler
  lazy val scheduler = StdSchedulerFactory.getDefaultScheduler()

  // calculate the jobs to run
  private def calcJobs: List[TriggeredJob] = {
    import Props.RunModes._

    Props.mode match {
      case Production => productionJobs
      case Staging => Nil
      case Pilot => nonproductionJobs
      case _ => nonproductionJobs
    }
  }

  def init(): Unit = {
    val js = jobs.vend
    if (js.length > 0) {
      js.foreach { tj => scheduler.scheduleJob(tj.detail, tj.trigger) }
      scheduler.start
    }
  }

  def shutdown(): Unit = {
    scheduler.shutdown
  }

  private def productionJobs: List[TriggeredJob] = {
    OneWeekNotifyGrowthJob ::
    DailyAgentSalesReportEmailJob ::
    DailyInternalReportEmailJob ::
    DailyTrackingEmailJob ::
    HalfHourCreateOrderJob ::
    Nil
  }

  private def nonproductionJobs: List[TriggeredJob] = {
    //FrequentNotifyGrowthJob ::
    //FrequentAgentSalesReportEmailJob ::
    //FrequentTrackingEmailJob ::
    FrequentCreateOrderJob ::
    Nil
  }
}

