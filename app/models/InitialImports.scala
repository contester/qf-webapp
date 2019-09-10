package models

import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

case class ExternalDatabases(studentWeb: DatabaseConfig[JdbcProfile], schoolWeb: DatabaseConfig[JdbcProfile])