package com.mypetdefense.jobs

import com.mypetdefense.actor._
import com.mypetdefense.model.{EmailReport, ReportType}
import com.mypetdefense.service.ReportingService
import net.liftweb.mapper.By
import org.quartz._

trait SalesAgentReportEmailJobTrait extends ManagedJob {
  def getDailyReport() = {
    val dailyAgentData   = ReportingService.findYesterdaySalesByAgent
    val monthlyAgentData = ReportingService.findYesterdayMTDSalesByAgent

    val dailyAgencyData   = ReportingService.findYesterdaySalesByAgency
    val monthlyAgencyData = ReportingService.findYesterdayMTDSalesByAgency

    val report = EmailReport.findAll(By(EmailReport.reportType, ReportType.DailyTPPAgentSalesReportEmail))
    val emails = report.flatMap(_.emailRecords.toList).map(_.email.get)

    emails.map { email =>
      EmailActor ! DailySalesEmail(
        dailyAgentData,
        monthlyAgentData,
        dailyAgencyData,
        monthlyAgencyData,
        email
      )
    }
  }

  def getMonthlyReport() = {
    val monthlyAgentData = ReportingService.findLastMonthSalesByAgent
    val monthlyAgencyData = ReportingService.findLastMonthSalesByAgency

    val report = EmailReport.findAll(By(EmailReport.reportType, ReportType.MonthlyTPPAgentSalesReportEmail))
    val emails = report.flatMap(_.emailRecords.toList).map(_.email.get)

    emails.map { email =>
      EmailActor ! MonthlySalesEmail(
        monthlyAgentData,
        monthlyAgencyData,
        email
      )
    }

  }
}
class DailySalesAgentReportEmailJob extends SalesAgentReportEmailJobTrait {
  def execute(context: JobExecutionContext): Unit = executeOp(context)(getDailyReport())
}

class MonthlySalesAgentReportEmailJob extends SalesAgentReportEmailJobTrait {
  def execute(context: JobExecutionContext): Unit = executeOp(context)(getMonthlyReport())
}

object MonthlyAgentSalesReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[MonthlySalesAgentReportEmailJob])
    .withIdentity("MonthlyAgentSalesReportEmailJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("MonthlyAgentSalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 1 * ? *"))
    .build()
}

object DailyAgentSalesReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[DailySalesAgentReportEmailJob])
    .withIdentity("DailyAgentSalesReportEmailJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyAgentSalesReportEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 7 ? * * *")) // sec, min, hour, DoM
    .build()
}

object FrequentDailyAgentSalesReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[DailySalesAgentReportEmailJob])
    .withIdentity("FrequentDailyAgentSalesReportEmailJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentDailyAgentSalesReportEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}

object FrequentMonthlyAgentSalesReportEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[MonthlySalesAgentReportEmailJob])
    .withIdentity("FrequentMonthlyAgentSalesReportEmailJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentMonthlyAgentSalesReportEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
