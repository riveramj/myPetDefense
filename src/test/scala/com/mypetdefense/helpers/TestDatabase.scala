package com.mypetdefense.helpers

sealed trait TestDatabase extends Product with Serializable {
  def isPostgreSQL: Boolean = false
  def isH2Memory: Boolean   = false
  def truncateAllQuery(tableNames: List[String]): String
}

case object PostgreSQL extends TestDatabase {
  override def isPostgreSQL: Boolean = true
  override def truncateAllQuery(tableNames: List[String]): String =
    tableNames.mkString("TRUNCATE ", ", ", ";")
}

case object H2Memory extends TestDatabase {
  override def isH2Memory: Boolean = true
  override def truncateAllQuery(tableNames: List[String]): String =
    tableNames.map(truncateQuery).mkString("\n")
  def truncateQuery(tableName: String): String =
    s"TRUNCATE TABLE $tableName RESTART IDENTITY;"
}
