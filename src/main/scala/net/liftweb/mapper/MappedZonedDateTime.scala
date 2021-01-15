package net.liftweb.mapper

import java.lang.reflect.Method
import java.sql.Types
import java.time.{Instant, ZonedDateTime}
import java.util.Date

import com.mypetdefense.AppConstants.DefaultTimezone
import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.{JE, JsExp}
import net.liftweb.http.{LiftRules, S}
import net.liftweb.json.{JInt, JValue, JsonAST}
import net.liftweb.util.Helpers.toDate
import net.liftweb.util.TimeHelpers.millis
import net.liftweb.util._

import scala.xml.{NodeSeq, Text}

abstract class MappedZonedDateTime[T <: Mapper[T]](
    val fieldOwner: T,
    val useNowAsDefault: Boolean = false
) extends MappedField[ZonedDateTime, T] {

  private val data    = FatLazy(defaultValue)
  private val orgData = FatLazy(defaultValue)

  private def date2zdt(date: Date): ZonedDateTime =
    if (date eq null) null else date.toInstant.atZone(DefaultTimezone)

  private def zdt2date(zdt: ZonedDateTime): Date =
    if (zdt eq null) null else Date.from(zdt.toInstant)

  def parse(s: String): Box[ZonedDateTime] =
    LiftRules.dateTimeConverter().parseDateTime(s).map(date2zdt)

  def format(dt: ZonedDateTime): String =
    LiftRules.dateTimeConverter().formatDateTime(zdt2date(dt))

  import scala.reflect.runtime.universe._
  override def manifest: TypeTag[ZonedDateTime] = typeTag[ZonedDateTime]

  override def sourceInfoMetadata(): SourceFieldMetadata { type ST = ZonedDateTime } =
    SourceFieldMetadataRep(
      name,
      manifest,
      new FieldConverter {
        override type T = ZonedDateTime

        override def asString(v: T): String                 = format(v)
        override def asNodeSeq(v: T): Box[NodeSeq]          = Full(Text(asString(v)))
        override def asJson(v: T): Box[JValue]              = Full(JInt(v.toInstant.toEpochMilli))
        override def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
      }
    )

  override protected def real_i_set_!(value: ZonedDateTime): ZonedDateTime = {
    if (value != data.get) {
      data() = value
      this.dirty_?(true)
    }
    data.get
  }

  override def dbFieldClass: Class[ZonedDateTime] = classOf[ZonedDateTime]

  override def asJsonValue: Box[JsonAST.JValue] =
    Full(get match {
      case null => JsonAST.JNull
      case v    => JsonAST.JInt(v.toInstant.toEpochMilli)
    })

  def toLong: Long =
    get match {
      case null              => 0L
      case dt: ZonedDateTime => dt.toInstant.toEpochMilli / 1000L
    }

  override def asJsExp: JsExp = JE.Num(toLong)

  override def targetSQLType: Int = Types.TIMESTAMP

  override def defaultValue: ZonedDateTime =
    if (useNowAsDefault) ZonedDateTime.now(DefaultTimezone) else null

  override def writePermission_? = true
  override def readPermission_?  = true

  override protected def i_is_! : ZonedDateTime       = data.get
  override protected def i_was_! : ZonedDateTime      = orgData.get
  override protected[mapper] def doneWithSave(): Unit = orgData.setFrom(data)

  override protected def i_obscure_!(in: ZonedDateTime): ZonedDateTime =
    Instant.ofEpochMilli(0L).atZone(DefaultTimezone)

  override def _toForm: Box[NodeSeq] =
    S.fmapFunc((s: List[String]) => this.setFromAny(s)) { funcName =>
      Full(
        appendFieldId(
          <input type={formInputType}
                                name={funcName}
                                value={
            get match {
              case null => ""
              case s    => format(s)
            }
          }/>
        )
      )
    }

  override def setFromAny(f: Any): ZonedDateTime =
    f match {
      case JsonAST.JNull                   => this.set(null)
      case JsonAST.JInt(v)                 => this.set(Instant.ofEpochMilli(v.longValue).atZone(DefaultTimezone))
      case n: Number                       => this.set(Instant.ofEpochMilli(n.longValue).atZone(DefaultTimezone))
      case "" | null                       => this.set(null)
      case s: String                       => parse(s).map(d => this.set(d)).openOr(this.get)
      case (s: String) :: _                => parse(s).map(d => this.set(d)).openOr(this.get)
      case d: Date                         => this.set(date2zdt(d))
      case Some(d: Date)                   => this.set(date2zdt(d))
      case Full(d: Date)                   => this.set(date2zdt(d))
      case dt: ZonedDateTime               => this.set(dt)
      case Some(dt: ZonedDateTime)         => this.set(dt)
      case Full(dt: ZonedDateTime)         => this.set(dt)
      case None | Empty | Failure(_, _, _) => this.set(null)
      case _                               => this.get
    }

  override def jdbcFriendly(field: String): Object =
    get match {
      case null => null
      case dt   => new java.sql.Timestamp(dt.toInstant.toEpochMilli)
    }

  override def real_convertToJDBCFriendly(value: ZonedDateTime): Object =
    if (value == null) null
    else new java.sql.Timestamp(value.toInstant.toEpochMilli)

  private def st(in: Box[ZonedDateTime]): Unit =
    in match {
      case Full(dt) => data.set(dt); orgData.set(dt)
      case _        => data.set(null); orgData.set(null)
    }

  override def buildSetActualValue(
      accessor: Method,
      v: AnyRef,
      columnName: String
  ): (T, AnyRef) => Unit =
    (inst, v) =>
      doField(
        inst,
        accessor, {
          case f: MappedZonedDateTime[T] => f.st(toDate(v).map(date2zdt))
        }
      )

  override def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) =>
      doField(inst, accessor, {
        case f: MappedZonedDateTime[T] =>
          f.st(if (isNull) Empty else Full(Instant.ofEpochMilli(v).atZone(DefaultTimezone)))
      })

  override def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
    (inst, v) =>
      doField(
        inst,
        accessor, {
          case f: MappedZonedDateTime[T] => f.st(toDate(v).map(date2zdt))
        }
      )

  override def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) =>
      doField(inst, accessor, {
        case f: MappedZonedDateTime[T] => f.st(Full(date2zdt(v)))
      })

  override def buildSetBooleanValue(
      accessor: Method,
      columnName: String
  ): (T, Boolean, Boolean) => Unit =
    (inst, _, _) => doField(inst, accessor, { case f: MappedZonedDateTime[T] => f.st(Empty) })

  override def fieldCreatorString(dbType: DriverType, colName: String): String =
    colName + " " + dbType.dateTimeColumnType + notNullAppender()

  def inFuture_? : Boolean =
    data.get match {
      case null => false
      case dt   => dt.toInstant.toEpochMilli > millis
    }

  def inPast_? : Boolean =
    data.get match {
      case null => false
      case dt   => dt.toInstant.toEpochMilli < millis
    }

  override def toString: String =
    if (get == null) "NULL" else format(get)
}
