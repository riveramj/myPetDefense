package com.mypetdefense.jobs

import net.liftweb._
import common._
import org.quartz.Scheduler
import org.quartz.impl.StdSchedulerFactory
import util._

object JobManager extends SimpleInjector with Loggable {

  // Quartz Scheduler
  lazy val scheduler: Scheduler                   = StdSchedulerFactory.getDefaultScheduler()
  val jobs: JobManager.Inject[List[TriggeredJob]] = new Inject(() => calcJobs) {}

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

  // calculate the jobs to run
  private def calcJobs: List[TriggeredJob] = {
    import Props.RunModes._

    Props.mode match {
      case Production => productionJobs
      case Staging    => Nil
      case Pilot      => nonproductionJobs
      case _          => nonproductionJobs
    }
  }

  private def productionJobs: List[TriggeredJob] = {
    //OneWeekNotifyGrowthJob ::
    DailyAgentSalesReportEmailJob ::
      DailyInternalReportEmailJob ::
      DailyTrackingEmailJob ::
      HalfHourCreateOrderJob ::
      OneTimePerDayDataIntegrityCheckJob ::
      Nil
  }

  private def nonproductionJobs: List[TriggeredJob] = {
    FrequentDataIntegrityCheckJob
    //FrequentNotifyGrowthJob ::
    //FrequentAgentSalesReportEmailJob ::
    //FrequentTrackingEmailJob ::
    //FrequentCreateOrderJob ::
    //FrequentTrackShipmentDeliveryJob ::
    //FrequentCreateTreatLabelJob ::
    Nil
  }
}
