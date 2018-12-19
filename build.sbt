import sbt._
import Keys._
import com.earldouglas.xsbtwebplugin._
import com.openstudy.sbt.ResourceManagementPlugin._

name := "My Pet Defense"

version := "1.0-RC1"

organization := "com.mypetdefense"

scalaVersion := "2.12.4"

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

seq(
  PluginKeys.start in WebPlugin.container.Configuration <<= 
    (compileSass in ResourceCompile)
      .dependsOn(copyScripts in ResourceCompile)
      .dependsOn(PluginKeys.start in WebPlugin.container.Configuration)
)

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= {
  val liftVersion = "3.2.0"
  Seq(
    "org.apache.shiro"    %  "shiro-core"         % "1.2.4",
    "ch.qos.logback"      %  "logback-classic"    % "1.2.3",
    "net.sf.opencsv"      %  "opencsv"            % "2.3",
    "org.postgresql"      %  "postgresql"         % "42.2.5",
    "org.quartz-scheduler" % "quartz"             % "2.3.0",
    "com.sun.activation"  % "javax.activation"    % "1.2.0",
    "org.eclipse.jetty"   %  "jetty-webapp"       % "9.4.12.v20180830" % "container; compile->default",
    "net.liftweb"         %% "lift-common"        % liftVersion % "compile",
    "net.liftweb"         %% "lift-webkit"        % liftVersion % "compile",
    "net.liftweb"         %% "lift-testkit"       % liftVersion % "test",
    "net.liftweb"         %% "lift-mapper"        % liftVersion % "compile",
    "me.frmr.stripe"      %% "streifen"           % "0.1.3-SNAPSHOT",
    "org.dispatchhttp"   %% "dispatch-core"        % "0.14.0",
    "org.dispatchhttp"   %% "dispatch-lift-json"   % "0.14.0",
    "com.mypetdefense.shipstation" %% "shipstation-scala" % "0.0.1-SNAPSHOT"
  )
}
