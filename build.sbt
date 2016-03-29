import sbt._
import Keys._
import com.earldouglas.xsbtwebplugin._
import com.openstudy.sbt.ResourceManagementPlugin._

name := "My Pet Defense"

version := "0.1-SNAPSHOT"

organization := "com.mypetdefense"

scalaVersion := "2.11.8"

seq(webSettings :_*)
seq(resourceManagementSettings :_*)

scalacOptions += "-deprecation"

seq(
  targetJavaScriptDirectory in ResourceCompile <<= (PluginKeys.webappResources in Compile) apply { resources => (resources / "static" / "js").get(0) },
  scriptDirectories in ResourceCompile <<= (PluginKeys.webappResources in Compile) map { resources => (resources / "javascript").get },
  styleDirectories in ResourceCompile <<= (PluginKeys.webappResources in Compile) map { resources => (resources / "static" / "css").get },
  // This is the same as the target above. Currently in production, we don't
  // deploy compressed scripts to S3, so we need them to live in the
  // same static files directory as we put dev JS files in during
  // development.
  compressedTarget in ResourceCompile <<= (PluginKeys.webappResources in Compile) apply { resources => (resources / "static").get(0) }
)

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= {
  val liftVersion = "2.6.3"
  Seq(
    "org.apache.shiro"    %  "shiro-core"         % "1.2.4",
    "ch.qos.logback"      %  "logback-classic"    % "1.1.3",
    "org.postgresql"      %  "postgresql"         % "9.4-1201-jdbc41",
    "org.eclipse.jetty"   %  "jetty-webapp"       % "9.2.13.v20150730" % "container; compile->default",
    "net.liftweb"         %% "lift-common"        % liftVersion % "compile",
    "net.liftweb"         %% "lift-webkit"        % liftVersion % "compile",
    "net.liftweb"         %% "lift-testkit"       % liftVersion % "test",
    "net.liftweb"         %% "lift-mapper"        % liftVersion % "compile",
    "me.frmr.stripe"      %% "streifen"           % "0.0.4-SNAPSHOT"
  )
}
